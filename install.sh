wget https://smlnj.org/dist/working/110.99.5/config.tgz
mkdir smlnj
tar -xf config.tgz -C smlnj
cd smlnj && config/install.sh && cd ..
export PATH=$(pwd)/smlnj/bin:$PATH
ml-build minisql.cm Main.main minisql-image



