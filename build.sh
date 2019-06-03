VERSION=v1.0.25

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
