const { expect } = require('chai')
const { Agent, db, document, endpoint, util } = require('../lib')

describe('document', function () {
  let agent

  before(function () {
    agent = new Agent().auth()
  })

  describe('1 database, shared', function () {
    let dbPath
    let docPath

    before(async function () {
      const dbDefaults = endpoint.db(agent.defaults())
      dbPath = dbDefaults.path
      docPath = endpoint.document(dbDefaults).path

      await db.createAfterDel(agent, dbPath)
    })

    after(async function () {
      await db.del(agent, dbPath)
    })

    describe('fails on bad schema @id (#647)', function () {
      const identifiers = [
        '',
        13,
        {},
        ['b'],
        false,
        true,
        null,
      ]
      for (const id of identifiers) {
        it(JSON.stringify(id), async function () {
          const schema = { '@id': id, '@type': 'Class' }
          const r = await document
            .insert(agent, docPath, { schema: schema })
            .then(document.verifyInsertFailure)
          expect(r.body['api:error']['@type']).to.equal('api:BadFieldValue')
          expect(r.body['api:error']['api:field']).to.equal('@id')
          expect(r.body['api:error']['api:value']).to.deep.equal(id)
          expect(r.body['api:error']['api:document']).to.deep.equal(schema)
        })
      }
    })

    describe('fails on bad schema @type', function () {
      const types = [
        '',
        13,
        {},
        ['b'],
        false,
        true,
        null,
      ]
      for (const type of types) {
        it(JSON.stringify(type), async function () {
          const schema = { '@id': util.randomString(), '@type': type }
          const r = await document
            .insert(agent, docPath, { schema: schema })
            .then(document.verifyInsertFailure)
          expect(r.body['api:error']['@type']).to.equal('api:BadFieldValue')
          expect(r.body['api:error']['api:field']).to.equal('@type')
          expect(r.body['api:error']['api:value']).to.deep.equal(type)
          expect(r.body['api:error']['api:document']).to.deep.equal(schema)
        })
      }
    })

    describe('handles strange schema @id', function () {
      const keys = [
        'false',
        'true',
        'null',
        '8',
        '[]',
        '{}',
        '/',
      ]
      for (const id of keys) {
        it(id, async function () {
          const schema = { '@id': id, '@type': 'Class' }
          await document
            .insert(agent, docPath, { schema: schema })
            .then(document.verifyInsertSuccess)
          const r = await document
            .get(agent, docPath, { query: { graph_type: 'schema', id: id } })
            .then(document.verifyGetSuccess)
          await document
            .del(agent, docPath, { query: { graph_type: 'schema', id: id } })
            .then(document.verifyDelSuccess)
          expect(r.body).to.deep.equal(schema)
        })
      }
    })

    it('responds with expected @id values (object)', async function () {
      const id = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: { '@type': 'Class', '@id': id },
        })
        .then(document.verifyInsertSuccess)
      await document
        .insert(agent, docPath, {
          instance: { '@type': id, '@id': `terminusdb:///data/${id}/0` },
        })
        .then(document.verifyInsertSuccess)
    })

    it('responds with expected @id values (array)', async function () {
      const id1 = util.randomString()
      const id2 = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: [
            { '@type': 'Class', '@id': id1 },
            { '@type': 'Class', '@id': id2 },
          ],
        })
        .then(document.verifyInsertSuccess)
      await document
        .insert(agent, docPath, {
          instance: [
            { '@type': id1, '@id': `terminusdb:///data/${id1}/1` },
            { '@type': id2, '@id': `terminusdb:///data/${id2}/2` },
          ],
        })
        .then(document.verifyInsertSuccess)
    })

    it('fails on subdocument @key checks (#566)', async function () {
      const schema = { '@type': 'Class', '@subdocument': [] }
      {
        schema['@id'] = util.randomString()
        const r = await document
          .insert(agent, docPath, { schema: schema })
          .then(document.verifyInsertFailure)
        expect(r.body['api:error']['@type']).to.equal('api:SubdocumentKeyMissing')
        expect(r.body['api:error']['api:document']['@id']).to.equal(schema['@id'])
      }
      {
        schema['@id'] = util.randomString()
        schema['@key'] = { useless_key: 'useless_value' }
        const r = await document
          .insert(agent, docPath, { schema: schema })
          .then(document.verifyInsertFailure)
        expect(r.body['api:error']['@type']).to.equal('api:DocumentKeyTypeMissing')
        expect(r.body['api:error']['api:document']['@id']).to.equal(schema['@id'])
        expect(JSON.parse(r.body['api:error']['api:key'])).to.deep.equal(schema['@key'])
      }
      {
        schema['@id'] = util.randomString()
        schema['@key'] = { '@type': 'Unknown' }
        const r = await document
          .insert(agent, docPath, { schema: schema })
          .then(document.verifyInsertFailure)
        expect(r.body['api:error']['@type']).to.equal('api:DocumentKeyTypeUnknown')
        expect(r.body['api:error']['api:document']['@id']).to.equal(schema['@id'])
        expect(r.body['api:error']['api:key_type']).to.equal(schema['@key']['@type'])
      }
    })

    it('fails when @key value is not an object (#587)', async function () {
      const schema = { '@id': util.randomString(), '@type': 'Class', '@key': false }
      const r = await document
        .insert(agent, docPath, { schema: schema })
        .then(document.verifyInsertFailure)
      expect(r.body['api:error']['@type']).to.equal('api:DocumentKeyNotObject')
      expect(r.body['api:error']['api:key_value']).to.equal(false)
      expect(r.body['api:error']['api:document']['@id']).to.equal(schema['@id'])
      expect(r.body['api:error']['api:document']['@key']).to.equal(false)
      expect(r.body['api:error']['api:document']['@type']).to.equal('Class')
    })

    it('handles different @id types (#622)', async function () {
      const type1 = util.randomString()
      const type2 = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: [
            { '@type': 'Class', '@id': type1 },
            { '@type': 'Class', '@id': type2, ref: type1 },
          ],
        })
        .then(document.verifyInsertSuccess)
      await document
        .insert(agent, docPath, {
          instance: [
            {
              '@type': type1,
              '@id': `terminusdb:///data/${type1}/1`,
            },
            {
              '@type': type2,
              '@id': `terminusdb:///data/${type2}/1`,
              ref: `terminusdb:///data/${type1}/1`,
            },
            {
              '@type': type2,
              '@id': `terminusdb:///data/${type2}/2`,
              ref: { '@id': `terminusdb:///data/${type1}/1` },
            },
            {
              '@type': type2,
              '@id': `terminusdb:///data/${type2}/3`,
              ref: { '@id': `terminusdb:///data/${type1}/1`, '@type': '@id' },
            },
          ],
        })
        .then(document.verifyInsertSuccess)
      const badValue = { field: 'abc' }
      {
        const r = await document
          .insert(agent, docPath, {
            instance: {
              '@type': type2,
              '@id': `terminusdb:///data/${type1}/4`,
              ref: badValue,
            },
          })
          .then(document.verifyInsertFailure)
        expect(r.body['api:error']['@type']).to.equal('api:MissingTypeField')
        expect(r.body['api:error']['api:document']).to.deep.equal(badValue)
      }
      {
        const r = await document
          .replace(agent, docPath, {
            instance: {
              '@type': type2,
              '@id': `terminusdb:///data/${type1}/1`,
              ref: badValue,
            },
          })
          .then(document.verifyReplaceFailure)
        expect(r.body['api:error']['@type']).to.equal('api:MissingTypeField')
        expect(r.body['api:error']['api:document']).to.deep.equal(badValue)
      }
    })

    it('fails for uexpected array value (#623)', async function () {
      const type = util.randomString()
      const expectedType = 'http://www.w3.org/2001/XMLSchema#string'
      await document
        .insert(agent, docPath, {
          schema: { '@id': type, '@type': 'Class', s: expectedType },
        })
        .then(document.verifyInsertSuccess)
      const badValue = ['a', 'b']
      const r = await document
        .insert(agent, docPath, {
          instance: { '@type': type, s: badValue },
        })
        .then(document.verifyInsertFailure)
      expect(r.body['api:error']['@type']).to.equal('api:UnexpectedArrayValue')
      expect(r.body['api:error']['api:value']).to.deep.equal(badValue)
      expect(r.body['api:error']['api:expected_type']).to.equal(expectedType)
    })

    it('fails for unexpected boolean values (#515)', async function () {
      const type = util.randomString()
      const expectedType = 'http://www.w3.org/2001/XMLSchema#string'
      await document
        .insert(agent, docPath, {
          schema: { '@id': type, '@type': 'Class', s: expectedType },
        })
        .then(document.verifyInsertSuccess)
      for (const value of [false, true]) {
        const r = await document
          .insert(agent, docPath, {
            instance: { '@type': type, s: value },
          })
          .then(document.verifyInsertFailure)
        expect(r.body['api:error']['@type']).to.equal('api:UnexpectedBooleanValue')
        expect(r.body['api:error']['api:value']).to.equal(value)
        expect(r.body['api:error']['api:expected_type']).to.equal(expectedType)
      }
    })

    it('does not stringify boolean literals (#723)', async function () {
      const type = util.randomString()
      const id = type + '/' + util.randomString()
      const schema = {
        '@id': type,
        '@type': 'Class',
        bfalse: 'xsd:boolean',
        btrue: 'xsd:boolean',
      }
      await document
        .insert(agent, docPath, {
          schema: schema,
        })
        .then(document.verifyInsertSuccess)
      await document
        .insert(agent, docPath, {
          instance: { '@type': type, '@id': id, bfalse: false, btrue: true },
        })
        .then(document.verifyInsertSuccess)
      const r = await document
        .get(agent, docPath, { query: { id: id } })
        .then(document.verifyGetSuccess)
      expect(r.body['@id']).to.equal(id)
      expect(r.body['@type']).to.equal(type)
      expect(r.body.bfalse).to.equal(false)
      expect(r.body.btrue).to.equal(true)
    })

    it('does not drop incoming links (#736)', async function () {
      const type1 = util.randomString()
      const type2 = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: [
            {
              '@type': 'Class',
              '@id': type1,
            },
            {
              '@type': 'Class',
              '@id': type2,
              // This class has a reference to the previous class.
              id1: { '@type': 'Optional', '@class': type1 },
            },
          ],
        })
        .then(document.verifyInsertSuccess)
      const doc1 = { '@type': type1, '@id': type1 + '/1' }
      const doc2 = { '@type': type2, '@id': type2 + '/2', id1: doc1['@id'] }
      await document
        .insert(agent, docPath, { instance: [doc1, doc2] })
        .then(document.verifyInsertSuccess)
      await document
        .replace(agent, docPath, { instance: doc1 })
        .then(document.verifyInsertSuccess)
      const r = await document
        .get(agent, docPath, { body: { id: doc2['@id'] } })
        .then(document.verifyGetSuccess)
      // Even though doc1 was replaced, doc2 should still have the same reference.
      expect(r.body).to.deep.equal(doc2)
    })

    it('it inserts if does not exist (#705)', async function () {
      const type1 = util.randomString()
      await document
        .insert(agent, docPath, {
          schema: [
            {
              '@type': 'Class',
              '@id': type1,
              '@key': {
                '@type': 'Lexical',
                '@fields': ['name'],
              },
              name: 'xsd:string',
            },
          ],
        })
        .then(document.verifyInsertSuccess)
      const doc1 = { '@type': type1, name: type1 + '/1' }
      await document
        .replace(agent, docPath, {
          instance: [
            doc1,
          ],
          create: 'true',
        })
        .then(document.verifyInsertSuccess)
      const r = await document
        .get(agent, docPath, { query: { type: type1 } })
        .then(document.verifyGetSuccess)
      // We have inserted doc1
      expect(r.body.name === type1 + '/1')
    })

    describe('key @fields', function () {
      const schema = { '@type': 'Class' }
      const keyTypes = [
        'Hash',
        'Lexical',
      ]

      beforeEach(function () {
        schema['@id'] = util.randomString()
      })

      for (const keyType of keyTypes) {
        it(`fails when @fields is missing for ${keyType}`, async function () {
          schema['@key'] = { '@type': keyType }
          const r = await document.insert(agent, docPath, { schema: schema })
          document.verifyInsertFailure(r)
          expect(r.body['api:error']['@type']).to.equal('api:KeyMissingFields')
          expect(r.body['api:error']['api:key_type']).to.equal(keyType)
          expect(r.body['api:error']['api:document']).to.deep.equal(schema)
        })

        it(`fails when @fields value is not an array for ${keyType}`, async function () {
          schema['@key'] = { '@type': keyType, '@fields': { key: 'value' } }
          const r = await document.insert(agent, docPath, { schema: schema })
          document.verifyInsertFailure(r)
          expect(r.body['api:error']['@type']).to.equal('api:KeyFieldsNotAnArray')
          expect(r.body['api:error']['api:fields']).to.deep.equal(schema['@key']['@fields'])
          expect(r.body['api:error']['api:document']).to.deep.equal(schema)
        })

        it(`fails when @fields value is empty array for ${keyType} (#727)`, async function () {
          schema['@key'] = { '@type': keyType, '@fields': [] }
          const r = await document.insert(agent, docPath, { schema: schema })
          document.verifyInsertFailure(r)
          expect(r.body['api:error']['@type']).to.equal('api:KeyFieldsIsEmpty')
          expect(r.body['api:error']['api:document']).to.deep.equal(schema)
        })
      }
    })
  })
})
