const { series, parallel, src, dest } = require('gulp');
const os = require('os')
const path = require('path')
const util = require('util')
const fs = require('fs')
const tmp = require('tmp')
const git = require('isomorphic-git')
const http = require('isomorphic-git/http/node')
const del = require('del')
const elmMake = require('node-elm-compiler').compile
const execa = require('execa');
const ts = require('gulp-typescript');

const config = {
    morphirJvmVersion: '0.7.1',
    morphirJvmCloneDir: tmp.dirSync()
}

const stdio = 'inherit';

async function clean() {
    return del([ 'dist' ])
}

async function cloneMorphirJVM() {
    return await git.clone({
        fs,
        http,
        dir: config.morphirJvmCloneDir.name,
        url: 'https://github.com/finos/morphir-jvm',
        ref: `tags/v${config.morphirJvmVersion}`,
        singleBranch: true
    })
}

function copyMorphirJVMAssets() {
    const sdkFiles = path.join(config.morphirJvmCloneDir.name, 'morphir/sdk/core/src*/**')
    return src([sdkFiles]).pipe(dest('redistributable/Scala/sdk'))
}

async function cleanupMorphirJVM() {
    return del(config.morphirJvmCloneDir.name + '/**', { force: true });
}

function make(rootDir, source, target) {
    return elmMake([source], { cwd: path.join(process.cwd(), rootDir), output: target })
}

function makeCLI() {
    return make('cli', 'src/Morphir/Elm/CLI.elm', 'Morphir.Elm.CLI.js')
}

function makeDevCLI() {
    return make('cli', 'src/Morphir/Elm/DevCLI.elm', 'Morphir.Elm.DevCLI.js')
}

function makeDevServer() {
    return make('cli', 'src/Morphir/Web/DevelopApp.elm', 'web/index.html')
}

function makeInsightAPI() {
    return make('cli', 'src/Morphir/Web/Insight.elm', 'web/insight.js')
}

function makeTryMorphir() {
    return make('cli', 'src/Morphir/Web/TryMorphir.elm', 'web/try-morphir.html')
}


const build =
    series(
        makeCLI,
        makeDevCLI,
        makeDevServer,
        makeInsightAPI,
        makeTryMorphir
    )


function morphirElmMake(projectDir, outputPath, options) {
    args = [ './cli/morphir-elm.js', 'make', '-p', projectDir, '-o', outputPath ]
    if (options.typesOnly) {
        args.push('--types-only')
    }
    console.log("Running: " + args.join(' '));
    return execa('node', args, { stdio })
}

function morphirElmGen(inputPath, outputDir, target) {
    args = [ './cli/morphir-elm.js', 'gen', '-i', inputPath, '-o', outputDir, '-t', target ]
    console.log("Running: " + args.join(' '));
    return execa('node', args, { stdio })
}


async function testUnit(cb) {
    await execa('elm-test');
}

function testIntegrationClean() {
    return del([
        'tests-integration/generated',
        'tests-integration/reference-model/morphir-ir.json'
    ])
}


async function testIntegrationMake(cb) {
    await morphirElmMake(
        './tests-integration/reference-model',
        './tests-integration/generated/refModel/morphir-ir.json')
}

async function testIntegrationMorphirTest(cb) {
    src('./tests-integration/generated/refModel/morphir-ir.json')
        .pipe(dest('./tests-integration/reference-model/'))
    await execa(
        'node',
        ['./cli/morphir-elm.js', 'test', '-p', './tests-integration/reference-model'],
        { stdio },
    )
}

async function testIntegrationGenScala(cb) {
    await morphirElmGen(
        './tests-integration/generated/refModel/morphir-ir.json',
        './tests-integration/generated/refModel/src/scala/',
        'Scala')
}

async function testIntegrationBuildScala(cb) {
    try {
        await execa(
            'mill', ['__.compile'],
            { stdio, cwd: 'tests-integration' },
        )
    } catch (err) {
        if (err.code == 'ENOENT') {
            console.log("Skipping testIntegrationBuildScala as `mill` build tool isn't available.");
        } else {
            throw err;
        }
    }
}

async function testIntegrationGenTypeScript(cb) {
    await morphirElmGen(
        './tests-integration/generated/refModel/morphir-ir.json',
        './tests-integration/generated/refModel/src/typescript/',
        'TypeScript')
}

function testIntegrationBuildTypeScript(cb) {
    return src('tests-integration/generated/refModel/src/typescript/**/*.ts')
        .pipe(ts({
            outFile: 'output.js'
        }))
        .pipe(dest('tests-integration/generated/refModel/src/typescript/output.js'));
}

const testIntegration = series(
        testIntegrationClean,
        testIntegrationMake,
        parallel(
            testIntegrationMorphirTest,
            series(
                testIntegrationGenScala,
                testIntegrationBuildScala,
            ),
            series(
                testIntegrationGenTypeScript,
                testIntegrationBuildTypeScript,
            ),
        )
    )


async function testMorphirIRMake(cb) {
    await morphirElmMake('.', 'tests-integration/generated/morphirIR/morphir-ir.json',
        { typesOnly: true })
}

async function testMorphirIRGenTypeScript(cb) {
    await morphirElmGen(
        './tests-integration/generated/morphirIR/morphir-ir.json',
        './tests-integration/generated/morphirIR/src/typescript/',
        'TypeScript')
}

function testMorphirIRBuildTypeScript(cb) {
    return src('tests-integration/generated/morphirIR/src/typescript/**/*.ts')
        .pipe(ts({
            outFile: 'output.js'
        }))
        .pipe(dest('tests-integration/generated/morphirIR/src/typescript/output.js'));
}

testMorphirIR = series(
    testMorphirIRMake,
    testMorphirIRGenTypeScript,
    testMorphirIRBuildTypeScript,
)


const test =
    parallel(
        testUnit,
        testIntegration,
        testMorphirIR,
    )

exports.clean = clean;
exports.makeCLI = makeCLI;
exports.makeDevCLI = makeDevCLI;
exports.build = build;
exports.test = test;
exports.testIntegration = testIntegration;
exports.testMorphirIR = testMorphirIR;
exports.default =
    series(
        clean,
        series(
            cloneMorphirJVM,
            copyMorphirJVMAssets,
            cleanupMorphirJVM
        ),
        build
    );
