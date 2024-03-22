wget https://smlnj.org/dist/working/110.99.5/config.tgz
mkdir smlnj
tar -xf config.tgz -C smlnj
cd smlnj && config/install.sh && cd ..
wget -O words.txt https://github.com/InnovativeInventor/dict4schools/raw/master/safedict_full.txt 
export PATH=$(pwd)/smlnj/bin:$PATH
ml-build minisql.cm Main.main minisql-image



