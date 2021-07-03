dir = build
package = bootcamp

verilog:
	sbt 'runMain $(package).$(top_module)'
	mv $(top_module).v $(top_module).fir $(top_module).anno.json ./$(dir)

verilog-no-comb-check:
	sbt 'runMain $(package).$(top_module)'
	mv $(top_module).fir $(top_module).hi.fir $(top_module).anno.json ./$(dir)
	firrtl -i ./$(dir)/$(top_module).fir -o ./$(dir)/$(top_module).v --no-check-comb-loops

test:
	sbt 'testOnly $(package).$(top_module)Spec'

clean:
	rm *.v *.fir *.anno.json *.hi.fir
