/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   Contributed by Jesse Millikan
   Modified by Matt Baker
   Ported, modified, and parallelized by Roman Pletnev
*/

ʼuse strictʼ;

var rd = require(ʼreadlineʼ), cp = require(ʼchild_processʼ);

function RefNum(num){ this.num = num; }
RefNum.prototype.toString = function() { return this.num.toString(); }

function frequency(seq, length){
    var freq = new Map(), n = seq.length-length+1, key, cur, i = 0;
    for(; i<n; ++i){
        key = seq.substr(i, length);
        cur = freq.get(key);
        cur === undefined ? freq.set(key, new RefNum(1)) : ++cur.num;
    }
    return freq;
}

function sort(seq, length){
    var f = frequency(seq, length), keys = Array.from(f.keys()),
        n = seq.length-length+1, res = ʼʼ;
    keys.sort((a, b)=>f.get(b)-f.get(a));
    for (var key of keys) res +=
        key.toUpperCase()+ʼ ʼ+(f.get(key)*100/n).toFixed(3)+ʼ\nʼ;
    res += ʼ\nʼ;
    return res;
}

function find(seq, s){
    var f = frequency(seq, s.length);
    return (f.get(s) || 0)+"\t"+s.toUpperCase()+ʼ\nʼ;
}

function master() {
    var workers = [];
    for (var i=1; i<5;++i) workers.push(
        cp.fork(__filename, [], {silent: true, env: {workerId: i}}));
    for (var w of workers) process.stdin.pipe(w.stdin);
    var jobs = workers.length, results = [];
    var messageHandler = function(i){
        return function(message){
            results[i] = message;
            if (!(--jobs)) {
                process.stdout.write(results.join(ʼʼ));
                process.exit(0);
            }
        };
    };
    for (var i=0; i<workers.length; ++i)
        workers[i].on(ʼmessageʼ, messageHandler(i));
}

function worker(){
    var seq = ʼʼ, reading = false;
    var lineHandler = function(line){
        if (reading) {
            if (line[0]!==ʼ>ʼ) seq += line;
        } else reading = line.substr(0, 6)===ʼ>THREEʼ;
    };
    rd.createInterface(process.stdin, process.stdout)
        .on(ʼlineʼ, lineHandler).on(ʼcloseʼ, function() {
            var res = ʼʼ;
            switch (process.env.workerId) {
                case ʼ1ʼ:
                    res += sort(seq, 1);
                    res += sort(seq, 2);
                    res += find(seq, "ggt");
                    break;
                case ʼ2ʼ:
                    res += find(seq, "ggta");
                    res += find(seq, "ggtatt");
                    break;
                case ʼ3ʼ:
                    res += find(seq, "ggtattttaatt");
                    break;
                case ʼ4ʼ:
                    res += find(seq, "ggtattttaatttatagt");
                    break;
            }
            process.send(res);
            process.exit();
        });
}

process.env.workerId ? worker() : master();

