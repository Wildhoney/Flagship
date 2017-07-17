import { readdirSync } from 'fs';
import { parse } from 'path';
import { titleCase } from 'change-case';
import Cryptr from 'cryptr';
import { generate } from 'shortid';

const secret = generate();
export const cryptr = new Cryptr(generate());

/**
 * @constant transformations
 * @type {Array}
 */
const transformations = [
    { from: 'Guinea Bissau', to: 'Guinea-Bissau' }
];

/**
 * @method exceptions
 * @param {String} name 
 * @return {String}
 */
const exceptions = name => {
    const model = transformations.find(item => item.from === name);
    return model ? model.to : name;
};

/**
 * @method isImage
 * @param {String} filename 
 * @return {Boolean}
 */
const isImage = filename => /svg$/i.test(filename);

/**
 * @method fetch
 * @return {Array}
 */
export const fetch = () => {
    return readdirSync(`${__dirname}/../images/flags`).filter(isImage).map(filename => {
        return { name: exceptions(titleCase(parse(filename).name)), flag: `${cryptr.encrypt(filename)}.svg` };
    });
}
