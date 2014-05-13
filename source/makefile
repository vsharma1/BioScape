./_build/main.byte:
	ocamlbuild main.byte

%.csv : %.spi ./_build/main.byte
	ocamlrun ./_build/main.byte $<

%.vis : %.spi_pos.csv
	python visualization/graphics.py $<
