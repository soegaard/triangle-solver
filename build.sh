echo "Copying triangle-solver.rkt to build/"
cp triangle-solver.rkt build/
echo "Running Whalesong on triangle-solver.rkt"
cd build
../../new-whalesong/whalesong/whalesong.rkt build triangle-solver.rkt
cd ..
echo "Injecting MathJax into triangle-solver.html"
racket -t insert-mathjax.rkt
mv build/triangle-solver.html.tmp build/triangle-solver.html
echo "Done. Use:"
echo "open build/triangle-solver.html"



