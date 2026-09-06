# Changelog

## 0.2.0.0 - 2026-09-06

### Toolchain
- Move from Stackage `lts-17.8` (GHC 8.10.4, hakyll 4.14) to `lts-24.57`
  (GHC 9.10.3, hakyll 4.16.8, pandoc 3). `hakyll` and `lrucache` no longer
  need `extra-deps`.
- Modernise `haspot.cabal` (cabal-version 2.4, `-Wall`, version bounds, drop
  unused `blaze-*`, `containers` and `filepath` dependencies).
- Pin the Docker base image to `nginx:stable-alpine` and add a
  `.dockerignore` so only `_site/` and `default.conf` enter the build context.

### Fixed
- Stylesheet link pointed at `customize-1622403341.css`, a file that does not
  exist in the repository, so local previews and fresh deploys rendered
  without styles. Templates and `about/index.html` now reference
  `customize.css`; the deploy script keeps doing cache-busting renames.
- Math never rendered: MathJax was loaded from `cdn.mathjax.org` (shut down in
  2017) and the `$mathjax$` field was not used by any template. The
  `\\(..\\)` / `\\[..\\]` reader extension was also enabled on the writer
  instead of the reader, and the extension chain only kept its last element.
  Math is now parsed correctly and MathJax 3 is loaded on pages that need it.
- Every page declared `<link rel="canonical">` and `og:url` pointing at the
  site root, telling search engines all posts duplicate the home page.
- `sitemap.xml` listed only the home and about pages; posts are now included
  with `<lastmod>`.
- Pagination compared the page index with `0` (indices start at `1`), so the
  first page emitted an empty "Newer Posts" slot. Replaced the hand-rolled
  pagination with Hakyll's `buildPaginateWith` / `paginateContext`.
- `teaser.html` had an unclosed `<div>` and a stray `</span>`, nesting the
  post body inside the meta line.
- `<time datetime>` carried the human-readable date; it now gets ISO 8601.
- `about/index.html`: mismatched `</h4>`, `<li>` items outside a list and a
  duplicated `id="publication"`.
- `cmd_haspot.sh`: no shebang, `echo "\n"` in help output, `brew install`
  on every deploy, unquoted paths, no validation of the post title, and the
  unknown-subcommand check could not fire. Rewritten with `set -euo pipefail`,
  `build`/`watch` subcommands, a portable `sed -i`, and configurable deploy
  directory / image name.
- `robots.txt` was empty; it is now generated with a `Sitemap:` line.
- The math demo in `posts/2011-11-04-octopress.md` was written for kramdown
  with every backslash doubled (`\\left`); with Pandoc's double-backslash
  math extension the formulas are taken verbatim, so the TeX inside the
  `\\[ .. \\]` blocks is now single-backslashed and renders.

### Changed
- `header.html` and `post-header.html` merged into `head.html`; navigation and
  footer extracted into `nav.html` / `footer.html` and shared by all pages.
  Posts and the archive page now show the site footer too.
- Navigation gained a "Home" link so post pages can reach the front page.
- `<title>` is `Post title · Blog title` on posts and `Archive · Blog title`
  on the archive; Open Graph `og:type` is `article` on posts.
- The copyright year in the footer is the build year instead of a hard-coded
  2019, and the author name comes from `conf/setting.yml`.
- `google_analytics_id` and the new `google_site_verification` are optional;
  when unset the corresponding tags are omitted.
- `stack run -- deploy` runs `./cmd_haspot.sh deploy`.
- Configuration errors print the YAML parse error and exit non-zero instead
  of "Configuration Error".
- Removed obsolete `X-UA-Compatible` / `Content-Type` meta tags and the
  unused `grids-responsive` stylesheet; added a feed `<link rel="alternate">`
  and a favicon link.

## 0.1.0.0

Initial Hakyll-based blog generator.
