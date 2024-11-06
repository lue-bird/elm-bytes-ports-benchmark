/**
 * This approach sends Bytes over a port to the Elm application
 * by converting it to a ascii string as an intermediate representation.
 */

/**
 * Receive the bytes from Elm
 * @returns {Promise<Uint8Array>}
 */
async function receive() {
  return new Promise((resolve) => {
    function callback(arr) {
      resolve(stringToBytes(arr));
      ports.asciiToJS.unsubscribe(callback);
    }
    ports.asciiToJS.subscribe(callback);
    ports.asciiTriggerSend.send(null);
  });
}

/**
 * Send bytes to Elm and returns the length as a sanity check
 * @param {Uint8Array} bytes
 * @returns {Promise<number>}
 */
async function send(bytes) {
  return new Promise((resolve) => {
    function callback(bytesLength) {
      resolve(bytesLength);
      ports.receivedBytes.unsubscribe(callback);
    }
    ports.receivedBytes.subscribe(callback);
    ports.asciiFromJS.send(bytesToString(bytes));
  });
}

/**
 * Convert string (in ascii format) to bytes
 * @param {string} str
 * @returns {Uint8Array}
 */
function stringToBytes(str) {
  const result = new Uint8Array(str.length);
  for (let i = 0; i < str.length; i++) {
    result[i] = str.charCodeAt(i)
  }
  return result;
}

/**
 * Convert Uint8Array to ascii string
 * @param {Uint8Array} bytes
 * @returns {number[]}
 */
function bytesToString(bytes) {
  let result = ""
  for (let i = 0; i < bytes.length; i++) {
    result += String.fromCharCode(bytes[i])
  }
  return result
}


export default { send, receive };
