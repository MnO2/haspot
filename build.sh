VERSION=v1.0.9

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
