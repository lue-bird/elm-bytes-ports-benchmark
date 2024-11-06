/**
 * This approach sends Bytes over a port to the Elm application
 * by converting it to a hex string as an intermediate representation.
 */

/**
 * Receive the bytes from Elm
 * @returns {Promise<Uint8Array>}
 */
async function receive() {
  return new Promise((resolve) => {
    function callback(arr) {
      resolve(stringToBytes(arr));
      ports.hexToJS.unsubscribe(callback);
    }
    ports.hexToJS.subscribe(callback);
    ports.hexTriggerSend.send(null);
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
    ports.hexFromJS.send(bytesToString(bytes));
  });
}

/**
 * Convert string (in hex format) to bytes
 * @param {string} str
 * @returns {Uint8Array}
 */
function stringToBytes(str) {
  const result = new Uint8Array(str.length / 2);
  for (let i = 0; i < result.length; i++) {
    result[i] = parseInt(str.slice(i * 2, i * 2 + 2), 16);
  }
  return result;
}

/**
 * Convert Uint8Array to hex string
 * @param {Uint8Array} bytes
 * @returns {number[]}
 */
function bytesToString(bytes) {
  let result = "";
  bytes.forEach((byte) => {
    result += byteToHex[byte]
  });
  return result;
}

const byteToHex = [];
for (let n = 0; n <= 0xff; ++n) {
  const hexOctet = n.toString(16).padStart(2, "0");
  byteToHex.push(hexOctet);
}


export default { send, receive, stringToBytes };
