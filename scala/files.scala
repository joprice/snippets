import java.nio.file._

def readBytes(fileName: String) = Files.readAllBytes(Paths.get(fileName))

def writeToFile(data: String, file: String) = Files.write(Paths.get(file), data.getBytes())
