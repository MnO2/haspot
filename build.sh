VERSION=v1.0.18

docker build -t paulmeng/blog:$VERSION .
docker push paulmeng/blog:$VERSION
