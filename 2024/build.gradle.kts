plugins {
    kotlin("jvm") version "2.0.21"

    id("com.diffplug.spotless") version "6.25.0"
}

group = "org.arno"
version = "1.0"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
}

spotless {
    kotlin {
        ktlint()
    }
}

tasks.test {
    useJUnitPlatform()
}