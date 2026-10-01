#!/usr/bin/env python3
"""Compare orgfdb parser output with Emacs org-element (issue #4).

Local use only: Emacs is not part of CI.  Requires Emacs (tested with 29.3 /
Org 9.6.15) and a built orgfdb binary.  Nothing is parsed by evaluating Lisp
from the Org files; see scripts/emacs-org-dump.el for the safety settings.

Usage:
  scripts/emacs-oracle.py emacs FILE       # JSON facts from Emacs
  scripts/emacs-oracle.py orgfdb FILE      # JSON facts from the orgfdb index
  scripts/emacs-oracle.py compare [FILE..] # diff both; default: all fixtures

Environment: EMACS (default emacs), ORGFDB_BIN (default target/release/orgfdb,
then target/debug/orgfdb).  Exit status of compare is 1 when a difference is
not classified in scripts/emacs-oracle-known.json (a: orgfdb bug, b: deviation of a
former backend, none left, c: intentional model difference; d, harness artifacts, are
normalized away in this script), else 0.
"""
import json
import os
import re
import shutil
import sqlite3
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DUMP_EL = ROOT / "scripts" / "emacs-org-dump.el"
KNOWN = ROOT / "scripts" / "emacs-oracle-known.json"


def emacs_facts(path):
    emacs = os.environ.get("EMACS", "emacs")
    expr = "(ofdb-dump %s)" % json.dumps(str(Path(path).resolve()))
    r = subprocess.run(
        [emacs, "--batch", "-Q", "-l", str(DUMP_EL), "--eval", expr],
        capture_output=True, check=False,
    )
    if r.returncode != 0:
        raise RuntimeError("emacs failed for %s: %s" % (path, r.stderr.decode()))
    d = json.loads(r.stdout.decode("utf-8"))
    # Harness: derive the file display title like the DB does (joined #+TITLE).
    titles = [v for k, v in d["file"]["keywords"] if k == "TITLE" and v]
    d["file"]["title"] = " ".join(titles) if titles else None
    # Harness: orgfdb stores an empty keyword value as NULL, Emacs as "".
    d["file"]["keywords"] = [[k, v or None] for k, v in d["file"]["keywords"]]
    for h in d["headings"]:
        # The tags table keeps no order and no empty tags (Org reports "" for ::a::).
        h["tags"] = sorted(t for t in h["tags"] if t)
    d["file"]["file_tags"] = sorted(d["file"]["file_tags"])
    return d


def sort_todo(d):
    """Harness normalization: compare TODO keywords as a set.  Emacs orders
    them per sequence and repeats keywords declared twice, orgfdb stores open
    keywords before closed ones, once each."""
    d["file"]["todo_keywords"] = sorted(
        {tuple(k) for k in d["file"]["todo_keywords"]})
    d["file"]["todo_keywords"] = [list(k) for k in d["file"]["todo_keywords"]]
    return d


def find_orgfdb():
    env = os.environ.get("ORGFDB_BIN")
    if env:
        return env
    for rel in ("target/release/orgfdb", "target/debug/orgfdb"):
        if (ROOT / rel).exists():
            return str(ROOT / rel)
    return shutil.which("orgfdb") or "orgfdb"


def orgfdb_facts(path):
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        (tmp / "src").mkdir()
        shutil.copy(path, tmp / "src" / "fixture.org")
        (tmp / "c.toml").write_text(
            'db_path = "x.sqlite"\n\n[[dirs]]\npath = "src"\n')
        r = subprocess.run(
            [find_orgfdb(), "rebuild", "--config", str(tmp / "c.toml")],
            capture_output=True, check=False)
        if r.returncode != 0:
            raise RuntimeError("orgfdb rebuild failed for %s: %s"
                               % (path, r.stderr.decode()))
        db = sqlite3.connect(str(tmp / "x.sqlite"))
        try:
            return read_db(db)
        finally:
            db.close()


def emacs_style_title(title_raw, todo, priority):
    """Harness normalization: title_raw is the source title area without tags
    (docs/cli.org), so it still holds the TODO keyword, the priority cookie
    and COMMENT.  Emacs :raw-value excludes them, so strip them here."""
    t = title_raw or ""
    for prefix in ([todo] if todo else []):
        if t.startswith(prefix):
            t = t[len(prefix):].lstrip(" \t")
    if priority and t.startswith("[#%s]" % priority):
        t = t[len(priority) + 3:].lstrip(" \t")
    if t == "COMMENT" or t.startswith(("COMMENT ", "COMMENT\t")):
        t = t[7:].lstrip(" \t")
    if priority and t.startswith("[#%s]" % priority):
        t = t[len(priority) + 3:].lstrip(" \t")
    return t


def read_db(db):
    q = lambda sql, *a: db.execute(sql, a).fetchall()

    def props(hid):
        return {k.upper(): v for k, v in q(
            "SELECT key, local_value FROM effective_properties "
            "WHERE heading_id=? AND local_value IS NOT NULL", hid)}

    def tags(hid):
        return sorted(t for (t,) in q("SELECT tag FROM tags WHERE heading_id=?", hid))

    def links(hid):
        return [{"type": t, "path": p, "search_option": so, "format": f,
                 "description": d, "raw": raw, "context": ctx}
                for t, p, so, f, d, raw, ctx in q(
                    "SELECT link_type, path, search_option, format, "
                    "raw_description, raw, source_context FROM links "
                    "WHERE heading_id=? ORDER BY byte_start", hid)]

    root = q("SELECT id, title_raw FROM headings WHERE level=0")[0]
    file = {
        "title": root[1],
        "keywords": [[k.upper(), v] for k, v in q(
            "SELECT keyword, value FROM keywords WHERE heading_id=? "
            "ORDER BY line_number, id", root[0])],
        "file_tags": tags(root[0]),
        "todo_keywords": [[k, t] for k, t in q(
            "SELECT keyword, state_type FROM todo_keywords ORDER BY sequence_no")],
        "properties": props(root[0]),
        "links": links(root[0]),
    }
    headings = []
    for (hid, level, line, title_raw, todo, ttype, prio, sched, dl, closed,
         arch) in q("SELECT id, level, line_number, title_raw, todo_keyword, "
                    "todo_type, priority, scheduled_raw, deadline_raw, "
                    "closed_raw, archivedp FROM headings WHERE level>0 "
                    "ORDER BY byte_start"):
        headings.append({
            "level": level, "line": line,
            "title": emacs_style_title(title_raw, todo, prio), "todo": todo,
            "todo_type": ttype, "priority": prio, "tags": tags(hid),
            "archived": bool(arch), "properties": props(hid),
            "scheduled": sched, "deadline": dl, "closed": closed,
            "links": links(hid),
        })
    return {"file": file, "headings": headings}


SCALARS = ["level", "title", "todo", "todo_type", "priority", "tags",
           "archived", "properties", "scheduled", "deadline", "closed"]


def diff_links(scope, e_links, o_links, out):
    o_left = list(o_links)
    for e in e_links:
        match = next((o for o in o_left if o["raw"] == e["raw"]), None)
        if match is None:
            out.append((scope, "link-missing-in-orgfdb", e["raw"], None))
            continue
        o_left.remove(match)
        for f in ("type", "path", "search_option", "format", "description"):
            if e[f] != match[f]:
                out.append((scope, "link.%s" % f, e[f], match[f], e["raw"]))
    for o in o_left:
        out.append((scope, "link-extra-in-orgfdb", None,
                    "%s [%s]" % (o["raw"], o["context"])))


def compare(e, o):
    out = []
    ef, of = e["file"], o["file"]
    for f in ("title", "keywords", "file_tags", "todo_keywords", "properties"):
        if ef[f] != of[f]:
            out.append(("file", f, ef[f], of[f]))
    diff_links("file", ef["links"], of["links"], out)
    e_by = {h["line"]: h for h in e["headings"]}
    o_by = {h["line"]: h for h in o["headings"]}
    for line in sorted(set(e_by) | set(o_by)):
        eh, oh = e_by.get(line), o_by.get(line)
        scope = "L%d" % line
        if eh is None or oh is None:
            out.append((scope, "heading-presence",
                        eh and eh["title"], oh and oh["title"]))
            continue
        scope = "L%d %s" % (line, (eh["title"] or "")[:30])
        for f in SCALARS:
            if eh[f] != oh[f]:
                out.append((scope, f, eh[f], oh[f]))
        diff_links(scope, eh["links"], oh["links"], out)
    return out


def load_known():
    try:
        return json.loads(KNOWN.read_text())
    except FileNotFoundError:
        return []


def classify(fixture, d, known):
    fact = d[1]
    text = " ".join(str(x) for x in d[2:])
    for rule in known:
        if (re.search(rule["fixture"], fixture) and re.fullmatch(rule["fact"], fact)
                and (not rule.get("scope") or re.search(rule["scope"], d[0]))
                and (not rule.get("text") or re.search(rule["text"], text))):
            return rule["category"], rule["note"]
    return None


def all_fixtures():
    files = sorted((ROOT / "tests/data/parser").rglob("fixture.org"))
    files += sorted((ROOT / "tests/data/emacs-oracle").glob("*.org"))
    return files


def main(argv):
    if len(argv) < 2 or argv[1] not in ("emacs", "orgfdb", "compare"):
        print(__doc__)
        return 2
    cmd = argv[1]
    if cmd == "emacs":
        print(json.dumps(emacs_facts(argv[2]), indent=1, ensure_ascii=False))
        return 0
    if cmd == "orgfdb":
        print(json.dumps(orgfdb_facts(argv[2]), indent=1, ensure_ascii=False))
        return 0
    files = [Path(a) for a in argv[2:] if not a.startswith("--")] or all_fixtures()
    known = load_known()
    new = 0
    total = 0
    for f in files:
        try:
            rel = str(f.resolve().relative_to(ROOT))
        except ValueError:
            rel = str(f)
        diffs = compare(sort_todo(emacs_facts(f)), sort_todo(orgfdb_facts(f)))
        print("== %s: %s" % (rel, "ok" if not diffs else "%d difference(s)" % len(diffs)))
        for d in diffs:
            total += 1
            c = classify(rel, d, known)
            tag = "[%s]" % c[0] if c else "[NEW]"
            new += 0 if c else 1
            extra = (" (%s)" % d[4]) if len(d) > 4 else ""
            print("  %s %s :: %s%s\n      emacs=%s\n      orgfdb=%s" % (
                tag, d[0], d[1], extra,
                json.dumps(d[2], ensure_ascii=False),
                json.dumps(d[3], ensure_ascii=False)))
            if c:
                print("      note: %s" % c[1])
    print("\n%d difference(s), %d unclassified" % (total, new))
    return 1 if new else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
