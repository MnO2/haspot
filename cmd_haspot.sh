VERSION=v1.0.26
PROGNAME=$(basename $0)

sub_new() {
  title=$1
  post_name=$(echo ${title// /-} | tr '[:upper:]' '[:lower:]')
  post_file_name=$(date +%Y-%m-%d)-$post_name
  article_datetime=$(date '+%Y-%m-%d %H:%M')
  cat <<EOF > posts/$post_file_name.md
---
layout: post
title: "$title"
date: $article_datetime 
comments: true
categories: 
---
EOF
}

sub_help() {
  echo "Usage: $PROGNAME <subcommand> [options]\n"
  echo "    new     -- generate new blog post"
  echo "    deploy  -- build a docker image and upload"
  echo ""
  echo "For help with each subcommand run:"
  echo "$PROGNAME <subcommand> -h|--help"
  echo ""
}

sub_deploy() {
  if ! [ -x "$(command -v docker)" ]; then
    echo 'Error: docker is not installed.' >&2
    exit 1
  fi

  docker build -t paulmeng/blog:$VERSION .
  docker push paulmeng/blog:$VERSION
}

subcommand=$1
case $subcommand in
  "" | "-h" | "--help")
    sub_help
    ;;
  *)
    shift
    sub_${subcommand} "$@"
    if [ $? = 127 ]; then
      echo "Error: '$subcommand' is not a known subcommand." >&2
      echo "       Run '$PROGNAME --help' for a list of known subcommands." >&2
      exit 1
    fi
    ;;
esac
