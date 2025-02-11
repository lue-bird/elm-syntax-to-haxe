using Elm;

function main():Void {
	trace("Running...");
	var input = Sys.stdin().readAll().toString();
	Sys.print(Elm.formatSingleModule_formatSingleModule(input));
}
