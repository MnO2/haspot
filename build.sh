VERSION=v1.0.24

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
