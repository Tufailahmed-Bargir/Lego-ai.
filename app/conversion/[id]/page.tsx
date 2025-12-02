"use client"

import { useState, useEffect, use } from "react"
import Link from "next/link"
import { Navbar } from "@/components/navbar"
import { Clock, Copy, Check, ArrowLeft, AlertTriangle } from "lucide-react"

interface Conversion {
  id: string
  legacyLanguage: string
  modernLanguage: string
  legacyCode: string
  convertedCode: string
  createdAt: string
  status: "success" | "error"
  errorMessage?: string
}

// Sample data for demo
const sampleConversions: Record<string, Conversion> = {
  "1": {
    id: "1",
    legacyLanguage: "cobol",
    modernLanguage: "c++",
    legacyCode: `IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

PROCEDURE DIVISION.
    DISPLAY "Hello, World!".
    STOP RUN.`,
    convertedCode: `cpp
#include <iostream>

int main() {
    std::cout << "Hello, World!" << std::endl;
    return 0;
}`,
    createdAt: "2025-05-20T18:41:00Z",
    status: "success",
  },
  "2": {
    id: "2",
    legacyLanguage: "cobol",
    modernLanguage: "rust",
    legacyCode: `IDENTIFICATION DIVISION.
PROGRAM-ID. HelloWorld.
ENVIRONMENT DIVISION.
DATA DIVISION.
PROCEDURE DIVISION.
    DISPLAY "Hello, world!".
    STOP RUN.`,
    convertedCode: `fn main() {
    println!("Hello, world!");
}`,
    createdAt: "2025-04-08T17:57:00Z",
    status: "success",
  },
  "3": {
    id: "3",
    legacyLanguage: "cobol",
    modernLanguage: "rust",
    legacyCode: `IDENTIFICATION DIVISION.
PROGRAM-ID. COMPLEX-APP.`,
    convertedCode: "",
    createdAt: "2025-04-08T17:57:00Z",
    status: "error",
    errorMessage: `Build Error: Failed to compile. The legacy code structure is too complex or contains unsupported syntax. Please review your input and try again with a simpler code block.`,
  },
}

export default function ConversionDetailsPage({ params }: { params: Promise<{ id: string }> }) {
  const { id } = use(params)
  const [conversion, setConversion] = useState<Conversion | null>(null)
  const [copiedLegacy, setCopiedLegacy] = useState(false)
  const [copiedConverted, setCopiedConverted] = useState(false)

  useEffect(() => {
    // Try to find in sessionStorage first
    const storedHistory = JSON.parse(sessionStorage.getItem("conversionHistory") || "[]")
    const storedConversion = storedHistory.find((c: Conversion) => c.id === id)

    if (storedConversion) {
      setConversion(storedConversion)
    } else if (sampleConversions[id]) {
      setConversion(sampleConversions[id])
    }
  }, [id])

  const formatDate = (dateString: string) => {
    const date = new Date(dateString)
    return date.toLocaleDateString("en-US", {
      month: "long",
      day: "numeric",
      year: "numeric",
      hour: "2-digit",
      minute: "2-digit",
      hour12: true,
    })
  }

  const copyToClipboard = async (text: string, type: "legacy" | "converted") => {
    try {
      await navigator.clipboard.writeText(text)
      if (type === "legacy") {
        setCopiedLegacy(true)
        setTimeout(() => setCopiedLegacy(false), 2000)
      } else {
        setCopiedConverted(true)
        setTimeout(() => setCopiedConverted(false), 2000)
      }
    } catch (err) {
      console.error("Failed to copy:", err)
    }
  }

  if (!conversion) {
    return (
      <div className="min-h-screen bg-gray-50">
        <Navbar />
        <main className="pt-20 pb-12">
          <div className="max-w-4xl mx-auto px-4 text-center py-16">
            <h1 className="text-2xl font-bold text-gray-700">Conversion not found</h1>
            <Link href="/history" className="text-[#A855F7] hover:text-[#9333EA] mt-4 inline-block">
              Back to History
            </Link>
          </div>
        </main>
      </div>
    )
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <Navbar />

      <main className="pt-20 pb-24">
        {/* Header */}
        <div className="text-center py-8 px-4 animate-fade-in">
          <h1 className="text-3xl md:text-4xl font-bold text-[#6366F1] mb-4">Conversion Details</h1>
        </div>

        <div className="max-w-4xl mx-auto px-4 animate-fade-in-delay">
          {/* Created Date */}
          <div className="flex items-center gap-2 text-sm text-gray-500 mb-6">
            <Clock className="w-4 h-4" />
            <span>Created {formatDate(conversion.createdAt)}</span>
          </div>

          {/* Language Cards */}
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4 mb-8">
            <div className="bg-gray-100 p-4 rounded-lg">
              <span className="text-sm text-gray-500">Legacy Language</span>
              <p className="text-lg font-semibold text-gray-900">{conversion.legacyLanguage}</p>
            </div>
            <div className="bg-gray-100 p-4 rounded-lg">
              <span className="text-sm text-gray-500">Modern Language</span>
              <p className="text-lg font-semibold text-gray-900">{conversion.modernLanguage}</p>
            </div>
          </div>

          {/* Error Banner */}
          {conversion.status === "error" && (
            <div className="bg-red-50 border border-red-200 rounded-lg p-4 mb-8 flex items-start gap-3">
              <AlertTriangle className="w-5 h-5 text-[#EF4444] flex-shrink-0 mt-0.5" />
              <div>
                <h3 className="font-semibold text-[#EF4444]">Conversion Failed</h3>
                <p className="text-sm text-red-700 mt-1">{conversion.errorMessage}</p>
              </div>
            </div>
          )}

          {/* Legacy Code */}
          <div className="bg-white rounded-lg shadow-md p-6 mb-6">
            <div className="flex justify-between items-center mb-4">
              <h2 className="text-lg font-semibold text-gray-900">Legacy Code</h2>
              <button
                onClick={() => copyToClipboard(conversion.legacyCode, "legacy")}
                className="flex items-center gap-1.5 text-sm text-gray-500 hover:text-gray-700 bg-gray-100 px-3 py-1.5 rounded-md hover:bg-gray-200 transition-colors"
                aria-label="Copy legacy code"
              >
                {copiedLegacy ? (
                  <>
                    <Check className="w-4 h-4 text-[#10B981]" />
                    <span className="text-[#10B981]">Copied!</span>
                  </>
                ) : (
                  <>
                    <Copy className="w-4 h-4" />
                    <span>Copy</span>
                  </>
                )}
              </button>
            </div>
            <pre className="bg-gray-50 border border-gray-200 text-gray-800 p-4 rounded-md font-mono text-sm overflow-auto max-h-64">
              <code>{conversion.legacyCode}</code>
            </pre>
          </div>

          {/* Converted Code */}
          {conversion.status === "success" && (
            <div className="bg-white rounded-lg shadow-md p-6 mb-6">
              <div className="flex justify-between items-center mb-4">
                <h2 className="text-lg font-semibold text-gray-900">Converted Code</h2>
                <button
                  onClick={() => copyToClipboard(conversion.convertedCode, "converted")}
                  className="flex items-center gap-1.5 text-sm text-gray-500 hover:text-gray-700 bg-gray-100 px-3 py-1.5 rounded-md hover:bg-gray-200 transition-colors"
                  aria-label="Copy converted code"
                >
                  {copiedConverted ? (
                    <>
                      <Check className="w-4 h-4 text-[#10B981]" />
                      <span className="text-[#10B981]">Copied!</span>
                    </>
                  ) : (
                    <>
                      <Copy className="w-4 h-4" />
                      <span>Copy</span>
                    </>
                  )}
                </button>
              </div>
              <pre className="bg-gray-50 border border-gray-200 text-gray-800 p-4 rounded-md font-mono text-sm overflow-auto max-h-64">
                <code className="text-[#3B82F6]">{conversion.convertedCode}</code>
              </pre>
            </div>
          )}

          {/* Documentation */}
          {conversion.status === "success" && (
            <div className="bg-gray-100 rounded-lg p-6 mt-8">
              <h2 className="text-xl font-semibold text-gray-900 mb-4">Documentation</h2>
              <div className="text-gray-700 leading-relaxed space-y-4">
                <p>
                  This C++ program is a simple &quot;Hello, World!&quot; application. Its purpose is to print the text
                  &quot;Hello, World!&quot; to the console. The program consists of a single function,{" "}
                  <strong>main()</strong>. This is the entry point of the program; execution begins here.
                </p>
                <p>
                  Within the <strong>main()</strong> function:{" "}
                  <code className="bg-gray-200 px-1 rounded">
                    std::cout {"<<"} &quot;Hello, World!&quot; {"<<"} std::endl;
                  </code>{" "}
                  This statement uses the standard output stream (std::cout) to print the string &quot;Hello,
                  World!&quot; to the console. <strong>std::endl</strong> inserts a newline character at the end of the
                  output, moving the cursor to the next line.
                </p>
                <p>
                  <code className="bg-gray-200 px-1 rounded">return 0;</code> This statement indicates that the program
                  executed successfully. A return value of 0 typically signifies successful program termination to the
                  operating system. Any other integer value usually indicates an error.
                </p>
                <p>
                  <strong>Example:</strong> When this program is compiled and executed, the console will display:{" "}
                  <code className="bg-gray-200 px-1 rounded">Hello, World!</code>
                </p>
              </div>
            </div>
          )}
        </div>

        {/* Back Button */}
        <Link
          href="/history"
          className="fixed bottom-6 left-6 bg-[#A855F7] text-white p-4 rounded-full shadow-lg hover:bg-[#9333EA] transition-colors flex items-center gap-2"
          aria-label="Back to history"
        >
          <ArrowLeft className="w-5 h-5" />
          <span className="hidden md:inline">Back to History</span>
        </Link>
      </main>
    </div>
  )
}
