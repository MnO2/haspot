VERSION=v1.0.14

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
