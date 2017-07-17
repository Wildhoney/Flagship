import http from 'http';
import express from 'express';
import { resolve } from 'path';
import { fetch, cryptr } from './countries';

const app = express();
const server = http.createServer(app);

app.get('/countries.json', (_, res) => res.send(JSON.stringify(fetch())));
app.get('/images/flags/:hash.svg', (req, res) => res.sendFile(resolve(`${__dirname}/../images/flags/${cryptr.decrypt(req.params.hash)}`)));
app.use(express.static(`${__dirname}/..`));
server.listen(process.env.PORT || 5000);
