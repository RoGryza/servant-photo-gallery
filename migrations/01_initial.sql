CREATE TABLE posts (
  post_id INTEGER PRIMARY KEY,
  post_created_at TEXT NOT NULL
);

CREATE TABLE media (
  post_id INTEGER NOT NULL,
  media_index INTEGER NOT NULL,
  file_name TEXT NOT NULL,
  caption TEXT NOT NULL,
  width INTEGER NOT NULL,
  height INTEGER NOT NULL,
  PRIMARY KEY (post_id, media_index),
  FOREIGN KEY (post_id) REFERENCES posts(post_id)
);
