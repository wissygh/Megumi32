dir = build
package = bootcamp

verilog:
	sbt 'runMain $(package).$(top_module)'
	mv $(top_module).v $(top_module).fir $(top_module).anno.json ./$(dir)

test:
	sbt 'testOnly $(package).$(top_module)Spec'

clean:
	rm *.v *.fir *.anno.json
