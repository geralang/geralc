
function eq(a, b) { // assumes that types match
    if(typeof a === "object") {
        if(a instanceof Array) {
            if(a.length !== b.length) { return false; }
            for(let i = 0; i < a.length; i += 1) {
                if(!eq(a[i], b[i])) { return false; }
            }
            return true;
        }
        for(const key of Object.keys(a)) {
            if(!(key in b)) { return false; }
            if(!eq(a[key] != b[key])) { return false; }
        }
        for(const key of Object.keys(b)) {
            if(!(key in a)) { return false; }
        }
        return true;
    }
    return a === b;
}
