@file:Suppress("OPT_IN_USAGE")

import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "2.1.0"
    kotlin("plugin.power-assert") version "2.1.0"
    application
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation(kotlin("test"))
    implementation(kotlin("test-junit"))
    implementation(kotlin("reflect"))
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.9.0")
    implementation("org.jetbrains.kotlinx:kotlinx-coroutines-test:1.9.0")
    implementation("com.sksamuel.aedile:aedile-core:1.3.1") // Needed for CompanyDetailsRepository
}

tasks.withType<KotlinCompile> {
    kotlinOptions.freeCompilerArgs =
        listOf("-Xcontext-receivers", "-Xopt-in=kotlinx.coroutines.ExperimentalCoroutinesApi")
}

java.sourceSets["test"].java {
    srcDir("src/main/kotlin")
}

powerAssert {
    functions = listOf("kotlin.assert", "kotlin.test.assertEquals")
}

// Uncomment to use Loom
tasks.withType<JavaCompile>().configureEach {
    options.compilerArgs.add("--enable-preview")
}

kotlin {
    jvmToolchain(21)
}
