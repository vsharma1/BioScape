set FSPATH="C:\Program Files\FSharp-1.9.2.8\bin\"

%FSPATH%fsyacc --ml-compatibility -o parser.fs parser.mly
%FSPATH%fslex  -o lexer.fs  lexer.mll

%FSPATH%fsc --no-warn 62 --standalone -o spimfs.exe random.fs typ.mli typ.ml env.mli env.ml io.mli io.ml value.mli value.ml pattern.mli pattern.ml action.mli action.ml process.mli process.ml subrecord.mli subrecord.ml substore.mli substore.ml choice.mli choice.ml species.mli species.ml imap.mli imap.ml record.mli record.ml store.mli store.ml definition.mli definition.ml environment.mli environment.ml heap.mli heap.ml term.mli term.ml simulator.mli simulator.ml parser.fs lexer.fs main.ml

pause
