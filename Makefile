all: repl.wasm

repl.wasm: repl.scm module/repl-environment.scm
	guild compile-wasm -L module --bundle -gruntime-modules -o repl.wasm repl.scm

serve: repl.wasm
	guile -c '((@ (hoot web-server) serve))'

clean:
	rm repl.wasm
