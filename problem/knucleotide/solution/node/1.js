/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   Contributed by Jesse Millikan
   Modified by Matt Baker
   Ported, modified, and parallelized by Roman Pletnev
*/

ʼuse strictʼ;

var rd = require(ʼreadlineʼ), cluster = require(ʼclusterʼ);

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
    for (var key of keys) res += key+ʼ ʼ+(f.get(key)*100/n).toFixed(3)+ʼ\nʼ;
    res += ʼ\nʼ;
    return res;
}

function find(seq, s){
    var f = frequency(seq, s.length);
    return (f.get(s) || 0)+"\t"+s+ʼ\nʼ;
}

function master() {
    var workers = [], reading = false, seq = ʼʼ, maxBuf = 2048;

    for (var i=0; i<4;++i) workers.push(cluster.fork({workerId: i}));

    var flush = function(close) {
        for (var w of workers) w.send(seq);
        if (close) for (var w of workers) w.send(ʼeofʼ);
        seq = ʼʼ;
    };
    var lineHandler = function(line){
        if (reading) {
            if (line[0] !== ʼ>ʼ &&
                    (seq += line.toUpperCase()).length >= maxBuf) flush();
        } else reading = line.substr(0, 6) === ʼ>THREEʼ;
    };
    rd.createInterface(process.stdin, process.stdout)
        .on(ʼlineʼ, lineHandler).on(ʼcloseʼ, function() { flush(true); });

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
    var seq = ʼʼ;
    process.on(ʼmessageʼ, function(message){
        if (message === ʼeofʼ) {
            var res = ʼʼ;
            switch (process.env.workerId) {
                case ʼ0ʼ:
                    res += sort(seq, 1);
                    res += sort(seq, 2);
                    res += find(seq, "GGT");
                    break;
                case ʼ1ʼ:
                    res += find(seq, "GGTA");
                    res += find(seq, "GGTATT");
                    break;
                case ʼ2ʼ:
                    res += find(seq, "GGTATTTTAATT");
                    break;
                case ʼ3ʼ:
                    res += find(seq, "GGTATTTTAATTTATAGT");
                    break;
            }
            process.send(res);
            process.exit();
        } else seq += message;
    });
}

cluster.isMaster ? master() : worker();

