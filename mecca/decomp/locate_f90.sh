F90RAW="mpif90"
case $F90RAW in
  mpif90)
     F90="`mpif90 -show | grep '\-l' | awk '{print $1}'`"
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
