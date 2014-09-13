
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
  console.log('store exit');
}

function jsReadHash(name,key,value) {
  
}