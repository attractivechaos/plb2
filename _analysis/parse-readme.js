#!/usr/bin/env k8

function* k8_readline(fn) {
	let buf = new Bytes();
	let file = new File(fn);
	while (file.readline(buf) >= 0) {
		yield buf.toString();
	}
	file.close();
	buf.destroy();
}

function main(args) {
	if (args.length == 0) {
		print("Usage: parse-readme.js <README.md>");
		return;
	}
	let state = 0, a = [];
	for (const line of k8_readline(args[0])) {
		if (/^\|\s*Label/.test(line)) {
			state = 1;
		} else if (state && line[0] == "|" && line[1] != ":") {
			let t = line.split("|");
			let m, b = [t[1]];
			for (let i = 5; i <= 8; ++i)
				b.push((m = /(\d+\.\d+)/.exec(t[i])) != null? parseFloat(m[1]) : 0.0);
			a.push(b);
		}
	}
	a.sort(function(x,y) { return (x[1]+x[2]) - (y[1]+y[2]) });
	for (let i = 0; i < a.length; ++i)
		print(a[i].join("\t"));
}

main(arguments);
