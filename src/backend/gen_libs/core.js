
function gera___eq(a, b) { // assumes that types match
    if(typeof a === "object") {
        if(a instanceof Array) {
            if(a.length !== b.length) { return false; }
            for(let i = 0; i < a.length; i += 1) {
                if(!eq(a[i], b[i])) { return false; }
            }
            return true;
        }
        for(const key of Object.keys(a)) { // assumes that objects have same keys
            if(!eq(a[key] != b[key])) { return false; }
        }
        return true;
    }
    return a === b;
}

const gera___hash = (function() {
    const random_int = () => {
        const upper = BigInt.asIntN(64, BigInt(Math.floor(Math.random() * Number.MAX_SAFE_INTEGER))) << 32n;
        const lower = BigInt.asIntN(32, BigInt(Math.floor(Math.random() * Number.MAX_SAFE_INTEGER)));
        return BigInt.asIntN(64, upper + lower);
    };
    const object_hashes = new WeakMap();
    return (data) => {
        if(typeof data === "object") {
            if(!object_hashes.has(data)) {
                const h = random_int();
                object_hashes.set(data, h);
                return h;
            }
            return object_hashes.get(data);
        }
        const d = typeof data === "string"? data : data.toString();
        let h = 0n;
        for(let i = 0; i < d.length; i += 1) {
            h = BigInt.asIntN(64, BigInt(d.charCodeAt(i)) + (h << 6n) + (h << 16n) - h);
        }
        return h;
    };
})();

const gera___stack = {
    trace: [],
    push: function(name, file, line) { this.trace.push({ name, file, line }); },
    pop: function() { this.trace.pop(); }
};

function gera___panic(message) {
    let err = `The program panicked: ${message}\nStack trace (latest call first):\n`;
    let i = gera___stack.trace.length - 1
    while(true) {
        const si = gera___stack.trace[i];
        if(i < 0) { break; }
        err += `${i} ${si.name} at ${si.file}:${si.line}\n`;
        if(i === 0) { break; }
        i -= 1;
    }
    throw new Error(err);
}

function gera___verify_index(index, length, file, line) {
    const final_index = index < 0? length + index : index;
    if(final_index < length) { return final_index; }
    gera___stack.push("<index>", file, line);
    gera___panic(`the index ${index} is out of bounds for an array of length ${length}`);
    return -1;
}

function gera___verify_integer_divisor(d, file, line) {
    if(d != 0n) { return; }
    gera___stack.push("<division>", file, line);
    gera___panic("integer division by zero");
}