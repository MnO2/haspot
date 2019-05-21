VERSION=v1.0.19

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
