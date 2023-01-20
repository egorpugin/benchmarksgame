/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Roman Pletnev
   based on C/Dart programs by Petr Prokhorenkov/Jos Hirth et al.
   multi thread by Andrey Filatkin
 */

const { Worker, isMainThread, parentPort, workerData } = require(ʼworker_threads
ʼ);

const LINE_LEN = 60;
const RANDOM_BUF_SIZE = LINE_LEN * 1000;
const OUT_BUF_SIZE = (LINE_LEN + 1) * 1000;
const OUT_SIZE = 128 * 1024;
const NEW_LINE = 10;
const ENCODING = ʼbinaryʼ;

const alu =
    ʼGGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGʼ +
    ʼGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAʼ +
    ʼGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAʼ +
    ʼAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATʼ +
    ʼCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACʼ +
    ʼCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGʼ +
    ʼCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAʼ;

const ac = [
    {s: ʼaʼ, p: 0.27}, {s: ʼcʼ, p: 0.12}, {s: ʼgʼ, p: 0.12},
    {s: ʼtʼ, p: 0.27}, {s: ʼBʼ, p: 0.02}, {s: ʼDʼ, p: 0.02},
    {s: ʼHʼ, p: 0.02}, {s: ʼKʼ, p: 0.02}, {s: ʼMʼ, p: 0.02},
    {s: ʼNʼ, p: 0.02}, {s: ʼRʼ, p: 0.02}, {s: ʼSʼ, p: 0.02},
    {s: ʼVʼ, p: 0.02}, {s: ʼWʼ, p: 0.02}, {s: ʼYʼ, p: 0.02}
];

const hs = [
    {s: ʼaʼ, p: 0.3029549426680}, {s: ʼcʼ, p: 0.1979883004921},
    {s: ʼgʼ, p: 0.1975473066391}, {s: ʼtʼ, p: 0.3015094502008}
];


function Out() {
    this.bufferSize = OUT_SIZE;
    this.buf = Buffer.allocUnsafe(this.bufferSize);
    this.ct = 0;
}

Out.prototype.writeString = function(str) {
    this.buf.write(str, this.ct, str.length, ENCODING);
    this.ct += str.length;
};

Out.prototype.writeBuffer = function(buffer, pos, len) {
    if (this.ct + len >= this.bufferSize) {
        this.flush();
    }
    buffer.copy(this.buf, this.ct, pos, pos + len);
    this.ct += len;
};

Out.prototype.flush = function() {
    process.stdout.write(this.buf.subarray(0, this.ct));
    this.ct = 0;
};


if (isMainThread) {
    mainThread();
} else {
    workerThread(workerData);
}

async function mainThread() {
    const n = +process.argv[2];
    const out = new Out();

    out.writeString(ʼ>ONE Homo sapiens alu\nʼ);
    repeatFasta(alu, n * 2, out);

    out.writeString(ʼ>TWO IUB ambiguity codes\nʼ);
    await randomFasta(ʼacʼ, n * 3, out);

    out.writeString(ʼ>THREE Homo sapiens frequency\nʼ);
    await randomFasta(ʼhsʼ, n * 5, out);

    out.flush();
}

function randomFasta(geneType, count, out) {
    return new Promise(resolve => {
        const cpus = 4;
        const workers = new Map();

        const results = new Map();
        let lastResult = 0;

        let lastInd = 0;

        for (let i = 0; i < cpus; i++) {
            const worker = new Worker(__filename, {workerData: {geneType}});
            const input = new SharedArrayBuffer(RANDOM_BUF_SIZE * Float64Array.B
YTES_PER_ELEMENT);
            const output = new SharedArrayBuffer(OUT_BUF_SIZE * Uint8Array.BYTES
_PER_ELEMENT);

            workers.set(worker, {input, output, len: 0});
            worker.postMessage({name: ʼsabʼ, data: {input, output}});

            worker.on(ʼmessageʼ, message => {
                const name = message.name;

                if (name === ʼreadyʼ) {
                    nextChunk(worker);
                } else if (name === ʼresultʼ) {
                    const {len, chunkInd} = message.data;
                    workers.get(worker).len = len;
                    results.set(chunkInd, worker);
                    processResults();
                }
            });
            worker.on(ʼexitʼ, () => {
                workers.delete(worker);
                if (workers.size === 0) {
                    resolve();
                }
            });
        }

        function nextChunk(worker) {
            if (count > 0) {
                const input = workers.get(worker).input;
                const chunk = Math.min(count, RANDOM_BUF_SIZE);

                generateRandom(input, chunk);
                worker.postMessage({name: ʼworkʼ, data: {count: chunk, chunkInd:
 lastInd}});
                count -= chunk;
                lastInd++;
            } else {
                worker.postMessage({name: ʼexitʼ});
            }
        }

        function processResults() {
            while (results.has(lastResult)) {
                const worker = results.get(lastResult);
                const {output, len} = workers.get(worker);
                results.delete(lastResult);

                out.writeBuffer(Buffer.from(output), 0, len);
                nextChunk(worker);
                lastResult++;
            }
        }
    });
}

function workerThread({geneType}) {
    const geneList = makeCumulative(geneType === ʼacʼ ? ac : hs);
    let input;
    let output;

    parentPort.on(ʼmessageʼ, message => {
        const name = message.name;

        if (name === ʼsabʼ) {
            input = new Float64Array(message.data.input);
            output = new Uint8Array(message.data.output);
            parentPort.postMessage({name: ʼreadyʼ});
        } else if (name === ʼworkʼ) {
            const {count, chunkInd} = message.data;

            const len = generateDna(geneList, input, output, count);
            parentPort.postMessage({name: ʼresultʼ, data: {len, chunkInd}});
        } else if (name === ʼexitʼ) {
            process.exit();
        }
    });
}

function repeatFasta(alu, count, out) {
    const buffer = Buffer.from(alu + alu.substr(0, LINE_LEN), ENCODING);
    const len = alu.length;
    let pos = 0;
    while (count > 0) {
        const line = Math.min(count, LINE_LEN);

        out.writeBuffer(buffer, pos, line);
        out.buf[out.ct++] = NEW_LINE;

        pos += line;
        if (pos > len) {
            pos -= len;
        }
        count -= line;
    }
}

function makeCumulative(ac) {
    const result = [];
    let p = 0;
    for (let i = 0; i < ac.length; ++i) {
        p += ac[i].p;
        result.push({
            c: p,
            sc: ac[i].s.charCodeAt(0)
        });
    }
    return result;
}

let last = 42;

function generateRandom(array, count) {
    const buf = new Float64Array(array);

    let l = last;
    for (let i = 0; i < count; i++) {
        l = (l * 3877 + 29573) % 139968;
        buf[i] = l / 139968;
    }
    last = l;
}

function generateDna(geneList, input, output, count) {
    let i = 0;
    let o = 0;
    while (count > 0) {
        const line = Math.min(count, LINE_LEN);

        for (let j = 0; j < line; j++) {
            output[o++] = randomCode(input[i++]);
        }
        output[o++] = NEW_LINE;

        count -= line;
    }
    return o;

    function randomCode(rand) {
        for (let j = 0; j < geneList.length; j++) {
            if (rand < geneList[j].c) {
                return geneList[j].sc;
            }
        }
        return geneList[geneList.length - 1].sc;
    }
}

