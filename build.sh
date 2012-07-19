cp triangle-solver.rkt build/
cd build
../../new-whalesong/whalesong/whalesong.rkt build triangle-solver.rkt
#sed -i '.tmp' '/<\/script>/ r mathjax-script.js' triangle-solver.html
cd ..
# TODO
# Now insert content of mathjax-script.js in build/triangle-solver.rkt
