import { readdirSync } from 'fs';
import { parse } from 'path';
import { titleCase } from 'change-case';

const map = [
    { from: 'Guinea Bissau', to: 'Guinea-Bissau' }
];

const exceptions = name => {
    const model = map.find(item => item.from === name);
    return model ? model.to : name;
};

export default () => {
    return readdirSync(`${__dirname}/../images/flags`).map(filename => {
        return { name: exceptions(titleCase(parse(filename).name)), flag: filename };
    });
}
