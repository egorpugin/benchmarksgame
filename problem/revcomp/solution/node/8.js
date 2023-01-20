/*  The Computer Language Benchmarks Game
    https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

    contributed by Joe Farro
    parts taken from solution contributed by
    Jos Hirth which was modified by 10iii
    modified by Roman Pletnev
    multi thread by Andrey Filatkin
*/

const { Worker, isMainThread, parentPort } = require(ʼworker_threadsʼ);
const os = require(ʼosʼ);

const smap = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,84,86,71,72,0,
    0,67,68,0,0,77,0,75,78,0,0,0,89,83,65,65,66,87,0,82,
    0,0,0,0,0,0,0,84,86,71,72,0,0,67,68,0,0,77,0,75,78,0,
    0,0,89,83,65,65,66,87,0,82];

const lineLen = 60;
const fullLineLen = lineLen + 1;
const caret = ʼ>ʼ.charCodeAt(0);
const endLine = ʼ\nʼ.charCodeAt(0);

if (isMainThread) {
    mainThread();
} else {
    workerThread();
}

async function mainThread() {
    let worker = null;
    let titleBuf = Buffer.allocUnsafe(lineLen);
    let titleBufPos = 0;
    let titleBufPartial = false;

    let dataArray = new SharedArrayBuffer(1 << 20);
    let dataBuf = Buffer.from(dataArray);
    let dataBufPos = 0;

    for await (let chunk of process.stdin) {
        await onChunk(chunk);
    }

    await onSection();
    await worker;

    async function onChunk(chunk) {
        const len = chunk.length;
        let i = 0;
        if (titleBufPartial) {
            const endI = chunk.indexOf(endLine, i);
            toTitleBuf(chunk, i, endI + 1);
            titleBufPartial = false;
            i += endI + 1;
        }
        const caretI = chunk.indexOf(caret, i);
        if (caretI === -1) {
            toBuf(chunk, i, len);
        } else {
            toBuf(chunk, i, caretI);
            i = caretI;
            await onSection();

            const endI = chunk.indexOf(endLine, i);
            if (endI !== -1) {
                toTitleBuf(chunk, i, endI + 1);
                return onChunk(chunk.subarray(endI + 1));
            } else {
                toTitleBuf(chunk, i, len);
                titleBufPartial = true;
            }
        }
    }

    function toTitleBuf(buffer, from, to) {
        buffer.copy(titleBuf, titleBufPos, from, to);
        titleBufPos += to - from;
    }

    function toBuf(buffer, from, to) {
        if (from === to) {
            return;
        }

        const len = to - from;
        while (dataBufPos + len > dataBuf.length) {
            const newArr = new SharedArrayBuffer(dataBuf.length * 2);
            const newBuf = Buffer.from(newArr);
            dataBuf.copy(newBuf, 0, 0, dataBufPos);
            dataArray = newArr;
            dataBuf = newBuf;
        }
        buffer.copy(dataBuf, dataBufPos, from, to);
        dataBufPos += len;
    }

    async function onSection() {
        if (titleBufPos === 0) {
            return;
        }

        await worker;

        worker = processData(titleBuf, titleBufPos, dataBuf, dataBufPos);

        titleBuf = Buffer.allocUnsafe(lineLen);
        titleBufPos = 0;
        titleBufPartial = false;

        dataArray = new SharedArrayBuffer(dataBuf.length);
        dataBuf = Buffer.from(dataArray);
        dataBufPos = 0;
    }

    function processData(titleBuf, titleBufLen, dataBuf, dataBufLen) {
        return new Promise(resolve => {
            const threads = Math.max(1, os.cpus().length - 2);
            const lines = Math.ceil(dataBufLen / fullLineLen);
            const dataLen = dataBufLen - lines;
            const chunkLen = Math.floor(dataLen / (2 * threads));

            let wait = 0;
            let bottomStart = 0;
            let bottomRealStart = 0;
            let topStart = dataLen;
            let topRealStart = topStart + Math.floor(topStart / lineLen);
            for (let i = 0; i < threads; i++) {
                const bottomFinish = i < threads - 1 ? bottomStart + chunkLen :
dataLen >> 1;
                const topFinish = i < threads - 1 ? topStart - chunkLen : dataLe
n >> 1;
                const bottomRealFinish = bottomFinish + Math.floor(bottomFinish
/ lineLen);
                const topRealFinish = topFinish + Math.floor(topFinish / lineLen
);

                const worker = new Worker(__filename);
                worker.postMessage({data: {
                    dataArray: dataBuf.buffer,
                    topFrom: topRealFinish,
                    bottomFrom: bottomRealStart,
                    topSize: topRealStart - topRealFinish,
                    bottomSize: bottomRealFinish - bottomRealStart
                }});
                worker.on(ʼexitʼ, () => {
                    wait--;
                    if (wait === 0) {
                        resolve();
                    }
                });
                wait++;

                bottomStart = bottomFinish;
                bottomRealStart = bottomRealFinish;
                topStart = topFinish;
                topRealStart = topRealFinish;
            }
        })
            .then(() => {
                process.stdout.write(titleBuf.subarray(0, titleBufLen));
                process.stdout.write(dataBuf.subarray(0, dataBufLen));
            });
    }
}

function workerThread() {
    parentPort.on(ʼmessageʼ, message => {
        writeBuf(message.data);
        process.exit();
    });

    function writeBuf({dataArray, topFrom, bottomFrom, topSize, bottomSize}) {
        const input = new Uint8Array(dataArray, topFrom, topSize);
        const output = new Uint8Array(dataArray, bottomFrom, bottomSize);

        let i = topSize - 1;
        let o = 0;
        while (i >= 0) {
            let char1 = input[i--];
            if (char1 === endLine) {
                char1 = input[i--];
            }
            let char2 = output[o++];
            if (char2 === endLine) {
                char2 = output[o++];
            }
            output[o - 1] = smap[char1];
            input[i + 1] = smap[char2];
        }
    }
}

