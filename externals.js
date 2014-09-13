
function unwrapPtr(value) {
  return value.f.f().pop();
}

function jsNewHash(name) {
  hashName=unwrapPtr(name);
  window[hashName]=new Array();
}

function jsStoreHash(name,key,value) {
  hashName=unwrapPtr(name);
  hashKey=unwrapPtr(key);
  window[hashName][hashKey]=value;
}

function jsReadHash(name,key) {
  hashName=unwrapPtr(name);
  hashKey=unwrapPtr(key);
  return window[hashName][hashKey];
}