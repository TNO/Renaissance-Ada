PREFIX=/tmp/ADALIB_DIR
FILE=libadalang-Linux-master.tar.gz
mkdir -p $PREFIX
aws s3 cp s3://adacore-gha-tray-eu-west-1/libadalang/$FILE . --sse=AES256
tar xzf $FILE -C $PREFIX
rm -f -v $FILE
