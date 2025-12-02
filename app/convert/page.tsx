"use client"

import { useState } from "react"
import { useRouter } from "next/navigation"
import { Navbar } from "@/components/navbar"
import { Loader2 } from "lucide-react"

export default function ConvertPage() {
  const router = useRouter()
  const [legacyLanguage, setLegacyLanguage] = useState("")
  const [modernLanguage, setModernLanguage] = useState("")
  const [legacyCode, setLegacyCode] = useState("")
  const [isConverting, setIsConverting] = useState(false)

  const handleConvert = async () => {
    if (!legacyCode.trim()) return

    setIsConverting(true)

    // Simulate conversion delay
    await new Promise((resolve) => setTimeout(resolve, 2000))

    // Store conversion data in sessionStorage for demo
    const conversionId = Date.now().toString()
    const conversionData = {
      id: conversionId,
      legacyLanguage: legacyLanguage || "cobol",
      modernLanguage: modernLanguage || "c++",
      legacyCode:
        legacyCode ||
        `IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

PROCEDURE DIVISION.
    DISPLAY "Hello, World!".
    STOP RUN.`,
      convertedCode: `#include <iostream>

int main() {
    std::cout << "Hello, World!" << std::endl;
    return 0;
}`,
      createdAt: new Date().toISOString(),
      status: "success",
    }

    // Get existing history or initialize
    const existingHistory = JSON.parse(sessionStorage.getItem("conversionHistory") || "[]")
    existingHistory.unshift(conversionData)
    sessionStorage.setItem("conversionHistory", JSON.stringify(existingHistory))

    setIsConverting(false)
    router.push(`/conversion/${conversionId}`)
  }

  return (
    <div className="min-h-screen bg-white">
      <Navbar />

      <main className="pt-20 pb-12">
        {/* Header */}
        <div className="text-center py-8 px-4 animate-fade-in">
          <h1 className="text-3xl md:text-4xl font-bold text-[#6366F1] mb-3">Convert Your Legacy Code</h1>
          <p className="text-gray-600 max-w-2xl mx-auto">
            Transform your outdated codebase into modern, maintainable solutions with our AI-powered converter.
          </p>
        </div>

        {/* Form */}
        <div className="max-w-4xl mx-auto px-4 animate-fade-in-delay">
          {/* Language Selectors */}
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-6">
            <div>
              <label htmlFor="legacy-language" className="block text-sm font-medium text-gray-700 mb-2">
                Legacy Language
              </label>
              <input
                id="legacy-language"
                type="text"
                placeholder="Ex: Cobol, Delphi, Perl etc"
                value={legacyLanguage}
                onChange={(e) => setLegacyLanguage(e.target.value)}
                className="w-full border border-gray-300 rounded-md p-3 bg-white focus:outline-none focus:ring-2 focus:ring-[#A855F7] focus:border-transparent transition-all"
              />
            </div>
            <div>
              <label htmlFor="modern-language" className="block text-sm font-medium text-gray-700 mb-2">
                Modern Language
              </label>
              <input
                id="modern-language"
                type="text"
                placeholder="Ex: C, C++, Python, JAVA"
                value={modernLanguage}
                onChange={(e) => setModernLanguage(e.target.value)}
                className="w-full border border-gray-300 rounded-md p-3 bg-white focus:outline-none focus:ring-2 focus:ring-[#A855F7] focus:border-transparent transition-all"
              />
            </div>
          </div>

          {/* Code Editor */}
          <div className="mb-8">
            <label htmlFor="legacy-code" className="block text-sm font-medium text-gray-700 mb-2">
              Legacy Code
            </label>
            <div className="border border-gray-300 rounded-lg overflow-hidden">
              {/* Editor Header */}
              <div className="bg-gray-100 px-4 py-2 flex items-center gap-2 border-b border-gray-300">
                <div className="w-3 h-3 rounded-full bg-red-500"></div>
                <div className="w-3 h-3 rounded-full bg-yellow-500"></div>
                <div className="w-3 h-3 rounded-full bg-green-500"></div>
                <span className="ml-2 text-sm text-gray-600">Code Editor</span>
              </div>
              {/* Editor Body */}
              <textarea
                id="legacy-code"
                value={legacyCode}
                onChange={(e) => setLegacyCode(e.target.value)}
                placeholder="Paste your legacy code here..."
                className="w-full h-80 bg-[#0f172a] text-white p-4 font-mono text-sm resize-none focus:outline-none placeholder-gray-400"
                aria-label="Legacy code input"
              />
            </div>
          </div>

          {/* Convert Button */}
          <button
            onClick={handleConvert}
            disabled={isConverting || !legacyCode.trim()}
            className="w-full max-w-md mx-auto block bg-gradient-to-r from-[#A855F7] to-[#6366F1] text-white py-4 rounded-lg text-lg font-semibold hover:opacity-90 transform hover:scale-[1.02] transition-all disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none shadow-lg"
            aria-label="Convert code"
          >
            {isConverting ? (
              <span className="flex items-center justify-center gap-2">
                <Loader2 className="w-5 h-5 animate-spin" />
                Converting...
              </span>
            ) : (
              "Convert Code"
            )}
          </button>
        </div>
      </main>
    </div>
  )
}
