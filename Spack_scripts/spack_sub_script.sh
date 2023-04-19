#!/bin/bash
#substitute the paths to NeatIBP installation

sed -i 's@/usr/local/lib@'"$SPASM_INSTALL_DIR/lib"'@' $NEATIBP_INSTALL_DIR/NeatIBP/default_settings.txt

sed -i 's@/usr/bin/Singular@'"$SINGULAR_INSTALL_DIR/bin"'@' $NEATIBP_INSTALL_DIR/NeatIBP/default_settings.txt

sed -i 's@$(dirname $( realpath ${BASH_SOURCE}))@'"$NEATIBP_INSTALL_DIR/NeatIBP"'@' $NEATIBP_INSTALL_DIR/NeatIBP/run.sh

sed -i 's@$(dirname $( realpath ${BASH_SOURCE}))@'"$NEATIBP_INSTALL_DIR/NeatIBP"'@' $NEATIBP_INSTALL_DIR/NeatIBP/monitor.sh

sed -i 's@$(dirname $( realpath ${BASH_SOURCE}))@'"$NEATIBP_INSTALL_DIR/NeatIBP"'@' $NEATIBP_INSTALL_DIR/NeatIBP/continue.sh


mkdir $install_ROOT/NeatIBP

cp $NEATIBP_INSTALL_DIR/NeatIBP/continue.sh $install_ROOT/NeatIBP/continue.sh

cp $NEATIBP_INSTALL_DIR/NeatIBP/run.sh $install_ROOT/NeatIBP/run.sh

cp $NEATIBP_INSTALL_DIR/NeatIBP/monitor.sh $install_ROOT/NeatIBP/monitor.sh

cp $NEATIBP_INSTALL_DIR/NeatIBP/default_settings.txt $install_ROOT/NeatIBP/config.txt

mkdir $install_ROOT/NeatIBP/outputs


