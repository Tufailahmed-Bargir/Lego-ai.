"use client"

import { useState, useEffect } from "react"
import Link from "next/link"
import { Navbar } from "@/components/navbar"
import { Clock, ArrowRight, AlertCircle } from "lucide-react"

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
const sampleConversions: Conversion[] = [
  {
    id: "1",
    legacyLanguage: "cobol",
    modernLanguage: "c++",
    legacyCode: `IDENTIFICATION DIVISION. PROGRAM-ID. HELLO-WORLD. PROCEDURE DIVISION. DISPLAY "Hello, World!". STOP RUN.`,
    convertedCode: `#include <iostream>\n\nint main() {\n    std::cout << "Hello, World!" << std::endl;\n    return 0;\n}`,
    createdAt: "2025-05-20T13:11:00Z",
    status: "success",
  },
  {
    id: "2",
    legacyLanguage: "cobol",
    modernLanguage: "rust",
    legacyCode: `IDENTIFICATION DIVISION. PROGRAM-ID. HelloWorld. ENVIRONMENT DIVISION. DATA DIVISION. PROCEDURE DIVISION. DISPLAY "Hello, world!". STOP RUN.`,
    convertedCode: `fn main() {\n    println!("Hello, world!");\n}`,
    createdAt: "2025-04-08T17:57:00Z",
    status: "success",
  },
  {
    id: "3",
    legacyLanguage: "cobol",
    modernLanguage: "rust",
    legacyCode: `IDENTIFICATION DIVISION. PROGRAM-ID. COMPLEX-APP.`,
    convertedCode: "",
    createdAt: "2025-04-08T17:57:00Z",
    status: "error",
    errorMessage: `Build Error Failed to compile Next.js (14.2.13) is outdated (learn more) ./node_modules\\.pnpm\\next@14.2.13_react-dom@18.3.1_react@18.3.1_sass@1.85.0\\node_modules\\next\\dist\\compiled\\client-only\\error.js 'client-only' cannot be imported from a Server Component module...`,
  },
]

export default function HistoryPage() {
  const [conversions, setConversions] = useState<Conversion[]>([])

  useEffect(() => {
    // Load from sessionStorage and merge with sample data
    const storedHistory = JSON.parse(sessionStorage.getItem("conversionHistory") || "[]")
    setConversions([...storedHistory, ...sampleConversions])
  }, [])

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

  return (
    <div className="min-h-screen bg-gray-50">
      <Navbar />

      <main className="pt-20 pb-12">
        {/* Header */}
        <div className="text-center py-8 px-4 animate-fade-in">
          <h1 className="text-3xl md:text-4xl font-bold text-[#6366F1]">Conversion History</h1>
        </div>

        {/* Conversion List */}
        <div className="max-w-4xl mx-auto px-4 space-y-4 animate-fade-in-delay">
          {conversions.length === 0 ? (
            <div className="text-center py-16">
              <div className="w-16 h-16 bg-gray-200 rounded-full flex items-center justify-center mx-auto mb-4">
                <Clock className="w-8 h-8 text-gray-400" />
              </div>
              <h3 className="text-xl font-semibold text-gray-700 mb-2">No conversions yet</h3>
              <p className="text-gray-500 mb-6">Start converting your legacy code to see history here.</p>
              <Link
                href="/convert"
                className="inline-block bg-[#A855F7] text-white px-6 py-3 rounded-lg font-semibold hover:bg-[#9333EA] transition-colors"
              >
                Start Converting
              </Link>
            </div>
          ) : (
            conversions.map((conversion) => (
              <Link
                key={conversion.id}
                href={`/conversion/${conversion.id}`}
                className={`block bg-white rounded-lg shadow-md p-5 border-l-4 ${
                  conversion.status === "error" ? "border-l-[#EF4444]" : "border-l-[#A855F7]"
                } hover:shadow-lg transition-shadow`}
              >
                <div className="flex justify-between items-start">
                  <div className="flex-1">
                    {/* Date */}
                    <div className="flex items-center gap-2 text-sm text-gray-500 mb-2">
                      <Clock className="w-4 h-4" />
                      <span>{formatDate(conversion.createdAt)}</span>
                    </div>

                    {/* Title */}
                    <h3 className="text-lg font-semibold text-gray-900 mb-1">Code Conversion</h3>

                    {/* Language Badge */}
                    <p className="text-sm text-gray-600 mb-3">
                      {conversion.legacyLanguage} → {conversion.modernLanguage}
                    </p>

                    {/* Code Preview or Error */}
                    {conversion.status === "error" ? (
                      <div className="flex items-start gap-2">
                        <AlertCircle className="w-4 h-4 text-[#EF4444] mt-0.5 flex-shrink-0" />
                        <p className="text-sm text-[#EF4444] font-mono line-clamp-2">{conversion.errorMessage}</p>
                      </div>
                    ) : (
                      <div className="border-l-4 border-[#A855F7] pl-3">
                        <p className="text-sm text-gray-600 font-mono line-clamp-2">{conversion.legacyCode}</p>
                      </div>
                    )}
                  </div>

                  {/* Arrow */}
                  <ArrowRight className="w-5 h-5 text-gray-400 flex-shrink-0 ml-4" />
                </div>
              </Link>
            ))
          )}

          {/* Load More Hint */}
          {conversions.length > 0 && <p className="text-center text-sm text-gray-400 pt-4">Load more...</p>}
        </div>
      </main>
    </div>
  )
}
