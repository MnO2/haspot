VERSION=v1.0.12

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
