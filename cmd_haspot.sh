VERSION=v1.0.50
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
  echo "    deploy  -- push it to github"
  echo "    docker  -- build a docker image and upload"
  echo ""
  echo "For help with each subcommand run:"
  echo "$PROGNAME <subcommand> -h|--help"
  echo ""
}

sub_deploy() {
  brew install coreutils
  TARGET_DIR=$(grealpath ../mno2.github.io)
  TIMESTAMP=$(date +%s)

  NEW_FILENAME=customize-$TIMESTAMP.css
  stack run rebuild
  cp -r _site/** $TARGET_DIR 
  cd $TARGET_DIR
  find . -name "*.html" -exec sed -i '' s/customize.css/$NEW_FILENAME/g {} +
  mv about/stylesheets/customize.css about/stylesheets/$NEW_FILENAME
  mv stylesheets/customize.css stylesheets/$NEW_FILENAME
  git add .
  git commit -m "New post"
  git push origin master
}

sub_docker() {
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
