
function eq(a, b) { // assumes that types match
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

const object_hashes = new WeakMap();

function random_int() {
    const upper = BigInt.asIntN(64, BigInt(Math.floor(Math.random() * Number.MAX_SAFE_INTEGER)))
        << 32n;
    const lower = BigInt.asIntN(32, BigInt(Math.floor(Math.random() * Number.MAX_SAFE_INTEGER)));
    return BigInt.asIntN(64, upper + lower);
}

function hash(data) {
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
}