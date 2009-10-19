F90RAW="mpif90"
case $F90RAW in
  mpif90)
     TMPMPIF90=`which mpif90`
     F90=`grep F90= $TMPMPIF90 | head -n 1 | awk -F '=' '{print $2}' | sed 's|"||g'`
     ;;
  sxmpif9*)
     F90="`echo $F90RAW | sed 's|mp||' | sed 's|sxi|sx|'`"
     ;;
  mpxlf9*)
     F90="`echo $F90RAW | sed 's|mp||'`"
     ;;
  *)
     F90=$F90RAW
     ;;
esac
