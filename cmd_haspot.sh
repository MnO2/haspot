#!/usr/bin/env bash
#
# cmd_haspot.sh -- helper commands for the haspot blog.
#
#   ./cmd_haspot.sh new "Post title"   create posts/YYYY-MM-DD-post-title.md
#   ./cmd_haspot.sh build              build the site into _site/
#   ./cmd_haspot.sh watch              build and serve locally with live reload
#   ./cmd_haspot.sh deploy             build and push to the GitHub Pages checkout
#   ./cmd_haspot.sh docker             build and push the nginx Docker image
#
# Environment overrides:
#   HASPOT_DEPLOY_DIR    checkout receiving the built site (default ../mno2.github.io)
#   HASPOT_DOCKER_IMAGE  image name for the docker subcommand (default paulmeng/blog)
set -euo pipefail

VERSION=v1.0.50
PROGNAME=$(basename "$0")
ROOT_DIR=$(cd "$(dirname "$0")" && pwd -P)
DEPLOY_DIR=${HASPOT_DEPLOY_DIR:-"$ROOT_DIR/../mno2.github.io"}
DOCKER_IMAGE=${HASPOT_DOCKER_IMAGE:-paulmeng/blog}

usage() {
  cat <<USAGE
Usage: $PROGNAME <subcommand> [args]

Subcommands:
  new <title>   Create a new Markdown post under posts/
  build         Build the site into _site/
  watch         Build the site and serve it at http://127.0.0.1:8000 with live reload
  deploy        Build and push _site/ to the GitHub Pages checkout
                (HASPOT_DEPLOY_DIR, currently: $DEPLOY_DIR)
  docker        Build and push the nginx Docker image ($DOCKER_IMAGE:$VERSION)
  help          Show this message
USAGE
}

# Turn "Hello, World: Again" into "hello-world-again": lower-case, spaces to
# dashes, and strip characters that are awkward in file names and URLs.
slugify() {
  printf '%s' "$1" \
    | tr '[:upper:]' '[:lower:]' \
    | tr -s ' ' '-' \
    | sed -e "s#[/\\\\:*?\"'<>|%&,;!\\#]##g" -e 's/-\{2,\}/-/g' -e 's/^-//' -e 's/-$//'
}

sub_new() {
  local title=${1:-}
  if [ -z "$title" ]; then
    echo "Error: a post title is required, e.g. $PROGNAME new \"Hello World\"" >&2
    exit 1
  fi

  local slug file yaml_title
  slug=$(slugify "$title")
  # Escape backslashes and double quotes so the title is a valid YAML string.
  yaml_title=${title//\\/\\\\}
  yaml_title=${yaml_title//\"/\\\"}
  file="$ROOT_DIR/posts/$(date +%Y-%m-%d)-$slug.md"
  if [ -e "$file" ]; then
    echo "Error: $file already exists." >&2
    exit 1
  fi

  # The date format below is one Hakyll parses directly (see getItemUTC), so
  # posts published on the same day keep their time-of-day order.
  cat <<POST > "$file"
---
title: "$yaml_title"
date: $(date '+%Y-%m-%dT%H:%M:%S%z')
---

POST
  echo "Created $file"
}

sub_build() {
  cd "$ROOT_DIR" && stack run -- build "$@"
}

sub_watch() {
  cd "$ROOT_DIR" && stack run -- watch "$@"
}

sub_deploy() {
  if [ ! -d "$DEPLOY_DIR/.git" ]; then
    echo "Error: $DEPLOY_DIR is not a git checkout; set HASPOT_DEPLOY_DIR." >&2
    exit 1
  fi

  local target stamp css
  target=$(cd "$DEPLOY_DIR" && pwd -P)
  stamp=$(date +%s)
  css="customize-$stamp.css"

  (cd "$ROOT_DIR" && stack run -- rebuild)
  cp -R "$ROOT_DIR/_site/." "$target/"

  cd "$target"
  # Cache-bust the stylesheet: give it a fresh name and rewrite every reference.
  rm -f stylesheets/customize-*.css about/stylesheets/customize-*.css
  mv stylesheets/customize.css "stylesheets/$css"
  mv about/stylesheets/customize.css "about/stylesheets/$css"
  find . -name '*.html' -not -path './.git/*' -exec sed -i.bak "s/customize\.css/$css/g" {} +
  find . -name '*.html.bak' -not -path './.git/*' -delete

  git add -A
  if git diff --cached --quiet; then
    echo "Nothing to deploy: the site is unchanged."
    return 0
  fi
  git commit -m "Deploy site $(date -u '+%Y-%m-%d %H:%M UTC')"
  git push origin master
}

sub_docker() {
  if ! command -v docker >/dev/null 2>&1; then
    echo 'Error: docker is not installed.' >&2
    exit 1
  fi
  [ -d "$ROOT_DIR/_site" ] || sub_build
  docker build -t "$DOCKER_IMAGE:$VERSION" "$ROOT_DIR"
  docker push "$DOCKER_IMAGE:$VERSION"
}

subcommand=${1:-help}
shift || true
case "$subcommand" in
  help|-h|--help)
    usage
    ;;
  new|build|watch|deploy|docker)
    "sub_$subcommand" "$@"
    ;;
  *)
    echo "Error: '$subcommand' is not a known subcommand." >&2
    usage >&2
    exit 1
    ;;
esac
