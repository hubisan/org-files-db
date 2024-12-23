;;;; * Insert

(defun org-files-db-database--insert-file (file)
  "Insert FILE metadata into the database and return the file ID."
  (let ((db org-files-db-database--connection)
        (modification-time (float-time (nth 5 (file-attributes file)))))
    (sqlite-execute db
                    "INSERT OR REPLACE INTO files (path, modification_time) VALUES (?, ?)"
                    (list file modification-time))
    (caar (sqlite-select db "SELECT id FROM files WHERE path = ?" (list file)))))

(defun org-files-db-database--insert-heading (file-id heading)
  "Insert HEADING data for FILE-ID into the database."
  (let ((db org-files-db-database--connection))
    (sqlite-execute db
                    "INSERT INTO headings (file_id, level, begin, title, title_raw, priority, todo_keyword, todo_type, archivedp, footnote_section_p, outline, all_tags, parent_id)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                    (list file-id
                          (plist-get heading :level)
                          (plist-get heading :begin)
                          (plist-get heading :title)
                          (plist-get heading :title-raw)
                          (plist-get heading :priority)
                          (plist-get heading :todo-keyword)
                          (plist-get heading :todo-type)
                          (plist-get heading :archivedp)
                          (plist-get heading :footnote-section-p)
                          (json-encode (plist-get heading :outline))
                          (json-encode (plist-get heading :all-tags))
                          (plist-get heading :parent-id)))))

(defun org-files-db-database--insert-link (file-id link)
  "Insert LINK data for FILE-ID into the database."
  (let ((db org-files-db-database--connection))
    (sqlite-execute db
                    "INSERT INTO links (heading_id, begin, type, path, path_absolute, raw_link, description, format, search_option)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
                    (list file-id
                          (plist-get link :begin)
                          (plist-get link :type)
                          (plist-get link :path)
                          (plist-get link :path-absolute)
                          (plist-get link :raw-link)
                          (plist-get link :description)
                          (plist-get link :format)
                          (plist-get link :search-option)))))

;;;; * Footer

(provide 'org-files-db-database)

;;; org-files-db-database.el ends here