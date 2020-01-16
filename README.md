# Servant Photo Gallery

Blog-like photo gallery API server.

## Quickstart

In order to run the server, you'll need an htpasswd file holding the users and a sqlite db with the
correct schema. Note that the first user in the htpasswd file is considered an admin and can upload
posts:

```shellsession
servant-photo-gallery$ htpasswd -nbBC 12 username password > htpasswd
servant-photo-gallery$ sqlite3 gallery.db < migrations/01_initial.sql
```

Now you can run a local instance with stack:

```shellsession
$ stack run
Starting gallery at 127.0.0.1:8000...
Showing media from static/media/
```

## Configuration

The server accepts the path to a configuration file as the first command line argument, it defaults
to `gallery.toml`. For a list of available settings check <src/PG/Config.hs>.

## Roadmap to v1.0

- Unit tests
- Allow passing configuration parameters through environment variables and the CLI.
- OpenAPI specs
- Remaining CRUD operations (delete and update posts)
- Media caching and streaming upload/download of files
- Handle proxies
- More descriptive HTTP error responses
- Add more logs and attach request ID and current user to them
- Publish to hackage and dockerhub

## Future Roadmap

- Better configuration validation (htpasswd file, base URL)
- Better configuration and startup error reporting in general
- More than one media per POST
- Other media types (Other image types, GIF/mp4)
- Image resizing on upload and on download per requested size restrictions
- Allow OAuth authentication
- Improve auth token handling (remember me, persist and rotate secrets)
- More fine-grained permissions
- Database/filesystem integrity checks
- Limits to prevent DoS: rate limits, file size limits
- CLI tool for administrative tasks
- Support for other databases and file storages
