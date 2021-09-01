#!/bin/sh

set -e

STARTDIR=$(pwd)

SRC_DIR='apps/elevators/src'
BLD_DIR='_build'
TMP_DIR='_tmp/elevators'

V1='1.0'
V2='2.0'

${TMP_DIR}/bin/elevators-${V1} stop || echo "No previous node running."
rm -rf ${TMP_DIR}
rm -rf ${BLD_DIR}

make rel1

echo "Backup version 1..."
mv -v ${SRC_DIR}/elevators.app.src ${SRC_DIR}/elevators.app.src.v1
mv -v ${SRC_DIR}/scheduler.erl ${SRC_DIR}/scheduler.erl.v1

echo "Copy over upgrade..."
cp -v upgrade/elevators.app.src ${SRC_DIR}
cp -v upgrade/scheduler.erl ${SRC_DIR}
cp -v upgrade/elevators.appup ${BLD_DIR}/default/lib/elevators/ebin/

make rel2

echo "Back to version 1..."
mv -v ${SRC_DIR}/elevators.app.src.v1 ${SRC_DIR}/elevators.app.src
mv -v ${SRC_DIR}/scheduler.erl.v1 ${SRC_DIR}/scheduler.erl

mkdir -pv ${TMP_DIR}
cp -v ${BLD_DIR}/default/rel/elevators/elevators-${V1}.tar.gz ${TMP_DIR}
cp -v ${BLD_DIR}/default/rel/elevators/elevators-${V2}.tar.gz ${TMP_DIR}

cd ${TMP_DIR}
tar xf elevators-${V1}.tar.gz

echo "Starting node..."
./bin/elevators-${V1} daemon

mkdir -pv releases/${V2}
cp -v elevators-${V2}.tar.gz releases/${V2}/elevators.tar.gz

sleep 5s # let the system boot

echo "Upgrading node to version 2..."
./bin/elevators-${V1} upgrade "${V2}/elevators"

rm -v elevators-${V1}.tar.gz
rm -v elevators-${V2}.tar.gz

cd "$STARTDIR"
