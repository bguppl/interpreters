import * as fs from 'fs';
import { format } from '../shared/format';

const readFilePromise = (filename: string): Promise<string> => {
  return new Promise<string>( (resolve, reject) => {
    fs.readFile(filename, (err, res) => {
      if (err)
        reject(err);
      else
        resolve(res.toString('utf8'));
    })
  })
}

const writeFilePromise = (filename: string, content: string): Promise<void> => {
  return new Promise( (resolve, reject) => {
    fs.writeFile(filename, content, (err) => {
      if (err)
        reject(err);
      else
        resolve();
    })
  })
}

// Chain the calls together
const readUpdateWrite = (filename: string): Promise<void> => {
    return readFilePromise(filename)
            .then((content) => {
                let j = JSON.parse(content);
                j.lastModified = new Date();
                return writeFilePromise(filename, format(j));
            })
            .catch((err) => console.error(err));
}

// The async/await version
const readUpdateWrite_async = async (filename: string): Promise<void> => {
    try {
        const content = await readFilePromise(filename);
        let j = JSON.parse(content);
        j.lastModified = new Date();
        return writeFilePromise(filename, format(j));
    }
    catch (err) {
        return console.error(err);
    }
}

writeFilePromise('test.async', format({a: 1}))
    .then(() => console.log("File is created"))
    .then(() => readFilePromise('test.async'))
    .then((content) => console.log(JSON.parse(content)))
    .then((content) => readUpdateWrite('test.async'))
    .then(() => console.log('File is updated'))
    .then(() => readFilePromise('test.async'))
    .then((content) => console.log(JSON.parse(content)));