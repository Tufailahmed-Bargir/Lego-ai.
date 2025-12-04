// "use client"

// import { useState } from "react"
// import { useRouter } from "next/navigation"
// import { Navbar } from "@/components/navbar"
// import { Loader2 } from "lucide-react"

// export default function ConvertPage() {
//   const router = useRouter()
//   const [legacyLanguage, setLegacyLanguage] = useState("")
//   const [modernLanguage, setModernLanguage] = useState("")
//   const [legacyCode, setLegacyCode] = useState("")
//   const [isConverting, setIsConverting] = useState(false)

//   const handleConvert = async () => {
//     if (!legacyCode.trim()) return

//     setIsConverting(true)

//     // Simulate conversion delay
//     await new Promise((resolve) => setTimeout(resolve, 2000))

//     // Store conversion data in sessionStorage for demo
//     const conversionId = Date.now().toString()
//     const conversionData = {
//       id: conversionId,
//       legacyLanguage: legacyLanguage || "cobol",
//       modernLanguage: modernLanguage || "c++",
//       legacyCode:
//         legacyCode ||
//         `IDENTIFICATION DIVISION.
// PROGRAM-ID. HELLO-WORLD.

// PROCEDURE DIVISION.
//     DISPLAY "Hello, World!".
//     STOP RUN.`,
//       convertedCode: `#include <iostream>

// int main() {
//     std::cout << "Hello, World!" << std::endl;
//     return 0;
// }`,
//       createdAt: new Date().toISOString(),
//       status: "success",
//     }

//     // Get existing history or initialize
//     const existingHistory = JSON.parse(sessionStorage.getItem("conversionHistory") || "[]")
//     existingHistory.unshift(conversionData)
//     sessionStorage.setItem("conversionHistory", JSON.stringify(existingHistory))

//     setIsConverting(false)
//     router.push(`/conversion/${conversionId}`)
//   }

//   return (
//     <div className="min-h-screen bg-white">
//       <Navbar />

//       <main className="pt-20 pb-12">
//         {/* Header */}
//         <div className="text-center py-8 px-4 animate-fade-in">
//           <h1 className="text-3xl md:text-4xl font-bold text-[#6366F1] mb-3">Convert Your Legacy Code</h1>
//           <p className="text-gray-600 max-w-2xl mx-auto">
//             Transform your outdated codebase into modern, maintainable solutions with our AI-powered converter.
//           </p>
//         </div>

//         {/* Form */}
//         <div className="max-w-4xl mx-auto px-4 animate-fade-in-delay">
//           {/* Language Selectors */}
//           <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-6">
//             <div>
//               <label htmlFor="legacy-language" className="block text-sm font-medium text-gray-700 mb-2">
//                 Legacy Language
//               </label>
//               <input
//                 id="legacy-language"
//                 type="text"
//                 placeholder="Ex: Cobol, Delphi, Perl etc"
//                 value={legacyLanguage}
//                 onChange={(e) => setLegacyLanguage(e.target.value)}
//                 className="w-full border border-gray-300 rounded-md p-3 bg-white focus:outline-none focus:ring-2 focus:ring-[#A855F7] focus:border-transparent transition-all"
//               />
//             </div>
//             <div>
//               <label htmlFor="modern-language" className="block text-sm font-medium text-gray-700 mb-2">
//                 Modern Language
//               </label>
//               <input
//                 id="modern-language"
//                 type="text"
//                 placeholder="Ex: C, C++, Python, JAVA"
//                 value={modernLanguage}
//                 onChange={(e) => setModernLanguage(e.target.value)}
//                 className="w-full border border-gray-300 rounded-md p-3 bg-white focus:outline-none focus:ring-2 focus:ring-[#A855F7] focus:border-transparent transition-all"
//               />
//             </div>
//           </div>

//           {/* Code Editor */}
//           <div className="mb-8">
//             <label htmlFor="legacy-code" className="block text-sm font-medium text-gray-700 mb-2">
//               Legacy Code
//             </label>
//             <div className="border border-gray-300 rounded-lg overflow-hidden">
//               {/* Editor Header */}
//               <div className="bg-gray-100 px-4 py-2 flex items-center gap-2 border-b border-gray-300">
//                 <div className="w-3 h-3 rounded-full bg-red-500"></div>
//                 <div className="w-3 h-3 rounded-full bg-yellow-500"></div>
//                 <div className="w-3 h-3 rounded-full bg-green-500"></div>
//                 <span className="ml-2 text-sm text-gray-600">Code Editor</span>
//               </div>
//               {/* Editor Body */}
//               <textarea
//                 id="legacy-code"
//                 value={legacyCode}
//                 onChange={(e) => setLegacyCode(e.target.value)}
//                 placeholder="Paste your legacy code here..."
//                 className="w-full h-80 bg-[#0f172a] text-white p-4 font-mono text-sm resize-none focus:outline-none placeholder-gray-400"
//                 aria-label="Legacy code input"
//               />
//             </div>
//           </div>

//           {/* Convert Button */}
//           <button
//             onClick={handleConvert}
//             disabled={isConverting || !legacyCode.trim()}
//             className="w-full max-w-md mx-auto block bg-gradient-to-r from-[#A855F7] to-[#6366F1] text-white py-4 rounded-lg text-lg font-semibold hover:opacity-90 transform hover:scale-[1.02] transition-all disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none shadow-lg"
//             aria-label="Convert code"
//           >
//             {isConverting ? (
//               <span className="flex items-center justify-center gap-2">
//                 <Loader2 className="w-5 h-5 animate-spin" />
//                 Converting...
//               </span>
//             ) : (
//               "Convert Code"
//             )}
//           </button>
//         </div>
//       </main>
//     </div>
//   )
// }



"use client";

import { useState } from "react";
import { useForm, SubmitHandler } from "react-hook-form";
import { zodResolver } from "@hookform/resolvers/zod";
import * as z from "zod";
import { toast, Toaster } from "sonner";
import { convertSchemaInput, ConvertType } from "@/lib/Schemas";
import axios from "axios";
// import Header from "@/components/header";
// import Footer from "@/components/footer";
import { Copy, Check } from "lucide-react";

export default function ConvertPage() {
  const {
    register,
    handleSubmit,
    formState: { errors },
  } = useForm<ConvertType>({
    resolver: zodResolver(convertSchemaInput),
  });

  const [outputCode, setOutputCode] = useState("");
  const [documentation, setDocumentation] = useState("");
  const [showForm, setShowForm] = useState(true);
  const [loading, setLoading] = useState(false);
  const [copiedField, setCopiedField] = useState<string | null>(null);

  const copyToClipboard = (text: string, field: string) => {
    navigator.clipboard.writeText(text);
    setCopiedField(field);
    toast.success("Text copied to clipboard!");
    setTimeout(() => setCopiedField(null), 2000);
  };

  const onSubmit: SubmitHandler<ConvertType> = async (data) => {
    setLoading(true);
    try {
      const response = await axios.post("/api/convert", data);

      if (response.data.success) {
        toast.success("Code converted successfully!");
        setOutputCode(response.data.generatedCode);
        setDocumentation(response.data.documentation);
        setShowForm(false);
      } else {
        toast.error("Error converting the code!");
      }
    } catch (error: any) {
      toast.error("Server error! Please try again.", error.message);
    } finally {
      setLoading(false);
    }
  };

  const CodeBlock = ({
    title,
    code,
    type,
  }: {
    title: string;
    code: string;
    type: string;
  }) => (
    <div className="border rounded-lg overflow-hidden bg-white shadow-sm">
      <div className="flex items-center border-b px-4 py-2 bg-gray-50">
        <div className="mr-2 h-3 w-3 rounded-full bg-red-500"></div>
        <div className="mr-2 h-3 w-3 rounded-full bg-yellow-500"></div>
        <div className="h-3 w-3 rounded-full bg-green-500"></div>
        <span className="ml-2 text-xs font-medium text-gray-600">{title}</span>
        <button
          onClick={() => copyToClipboard(code, type)}
          className="ml-auto inline-flex items-center px-2 py-1 text-xs bg-white hover:bg-gray-100 text-gray-600 rounded-md font-medium transition-colors duration-200"
        >
          {copiedField === type ? (
            <>
              <Check className="h-3.5 w-3.5 mr-1" />
              Copied!
            </>
          ) : (
            <>
              <Copy className="h-3.5 w-3.5 mr-1" />
              Copy
            </>
          )}
        </button>
      </div>
      <div
        className="bg-gray-900 p-4 font-mono text-sm text-gray-300"
        style={{ minHeight: "300px" }}
      >
        <pre className="overflow-auto">{code}</pre>
      </div>
    </div>
  );

  return (
    <div className="flex min-h-screen flex-col">
      {/* <Header /> */}
      <main className="flex-1 bg-gradient-to-b from-white to-gray-50">
        <div className="container mx-auto px-4 py-12">
          <Toaster position="top-right" closeButton richColors />

          <div className="max-w-6xl mx-auto">
            {showForm ? (
              <div className="space-y-8">
                <div className="text-center">
                  <h1 className="text-4xl font-bold mb-4 bg-gradient-to-r from-purple-600 to-blue-600 bg-clip-text text-transparent">
                    Convert Your Legacy Code
                  </h1>
                  <p className="text-gray-600 max-w-2xl mx-auto">
                    Transform your outdated codebase into modern, maintainable
                    solutions with our AI-powered converter.
                  </p>
                </div>

                <form onSubmit={handleSubmit(onSubmit)} className="space-y-8">
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                    <div className="space-y-2">
                      <label className="text-sm font-medium text-gray-700">
                        Legacy Language
                      </label>
                      <div className="relative">
                        <input
                          {...register("legacyLanguage")}
                          placeholder="Ex: Cobol, Delphi, Perl etc"
                          className="w-full px-4 py-3 bg-white border border-gray-200 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent transition"
                        />
                        {errors.legacyLanguage && (
                          <p className="mt-1 text-sm text-red-500">
                            {errors.legacyLanguage.message}
                          </p>
                        )}
                      </div>
                    </div>

                    <div className="space-y-2">
                      <label className="text-sm font-medium text-gray-700">
                        Modern Language
                      </label>
                      <div className="relative">
                        <input
                          {...register("modernLanguage")}
                          placeholder="Ex: C, C++, Python, JAVA"
                          className="w-full px-4 py-3 bg-white border border-gray-200 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent transition"
                        />
                        {errors.modernLanguage && (
                          <p className="mt-1 text-sm text-red-500">
                            {errors.modernLanguage.message}
                          </p>
                        )}
                      </div>
                    </div>
                  </div>

                  <div className="space-y-2">
                    <label className="text-sm font-medium text-gray-700">
                      Legacy Code
                    </label>
                    <div className="border rounded-lg overflow-hidden bg-white shadow-sm">
                      <div className="flex items-center border-b px-4 py-2 bg-gray-50">
                        <div className="mr-2 h-3 w-3 rounded-full bg-red-500"></div>
                        <div className="mr-2 h-3 w-3 rounded-full bg-yellow-500"></div>
                        <div className="h-3 w-3 rounded-full bg-green-500"></div>
                        <span className="ml-2 text-xs font-medium text-gray-600">
                          Code Editor
                        </span>
                      </div>
                      <textarea
                        {...register("code")}
                        className="w-full p-4 bg-gray-900 font-mono text-sm text-gray-300 border-none focus:ring-0 resize-none"
                        rows={12}
                        placeholder="Paste your legacy code here..."
                      />
                    </div>
                    {errors.code && (
                      <p className="text-sm text-red-500">
                        {errors.code.message}
                      </p>
                    )}
                  </div>

                  <div className="flex justify-center">
                    <button
                      type="submit"
                      disabled={loading}
                      className="px-8 py-4 bg-gradient-to-r from-purple-600 to-blue-600 text-white rounded-lg font-semibold hover:from-purple-700 hover:to-blue-700 transition-all duration-200 disabled:opacity-50 disabled:cursor-not-allowed min-w-[200px]"
                    >
                      {loading ? (
                        <div className="flex items-center justify-center">
                          <div className="w-5 h-5 border-t-2 border-b-2 border-white rounded-full animate-spin mr-2" />
                          Converting...
                        </div>
                      ) : (
                        "Convert Code"
                      )}
                    </button>
                  </div>
                </form>
              </div>
            ) : (
              <div className="space-y-12">
                <div className="text-center">
                  <h2 className="text-3xl font-bold mb-4 bg-gradient-to-r from-green-600 to-blue-600 bg-clip-text text-transparent">
                    Conversion Complete!
                  </h2>
                  <p className="text-gray-600 max-w-2xl mx-auto mb-8">
                    Your code has been successfully converted. Review the
                    results below.
                  </p>
                  <button
                    onClick={() => setShowForm(true)}
                    className="px-6 py-2 text-sm border border-gray-200 rounded-lg hover:bg-gray-50 transition-colors duration-200"
                  >
                    Convert Another Code
                  </button>
                </div>

                <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
                  <CodeBlock
                    title="Converted Code"
                    code={outputCode}
                    type="converted"
                  />
                  <div className="space-y-4">
                    <h3 className="text-xl font-semibold text-gray-800">
                      Documentation
                    </h3>
                    <div className="p-6 bg-white border border-gray-200 rounded-lg prose prose-gray max-w-none">
                      {documentation}
                    </div>
                  </div>
                </div>
              </div>
            )}
          </div>
        </div>
      </main>
      {/* <Footer /> */}
    </div>
  );
}
