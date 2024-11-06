/**
 * This approach sends Bytes over a port to the Elm application
 * by converting it to an array of numbers as an intermediate representation.
 */

/**
 * Receive the bytes from Elm
 * @returns {Promise<Uint8Array>}
 */
async function receive() {
  return new Promise((resolve) => {
    function callback(arr) {
      resolve(Uint8Array.from(arr));
      ports.intArrayToJS.unsubscribe(callback);
    }
    ports.intArrayToJS.subscribe(callback);
    ports.intArrayTriggerSend.send(null);
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
    ports.intArrayFromJS.send(Array.from(bytes));
  });
}

export default { send, receive };
