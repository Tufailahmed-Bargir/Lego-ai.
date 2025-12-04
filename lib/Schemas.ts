import { z } from "zod";

export const convertSchemaInput = z.object({
  legacyLanguage: z.string().min(1, { message: "legacy Language is required" }),

  code: z.string().min(1, { message: "legacy  Code is required" }),

  modernLanguage: z.string().min(1, { message: "Modern Language is required" }),
});

export const signupSchema = z.object({
  name: z.string().min(4, "Username must be at least 4 characters"),
  email: z.string().email("Invalid email address"),
  password: z.string().min(4, "Password must be at least 6 characters"),
});

export const LoginSchema = z.object({
  email: z.string().email("Invalid email address"),
  password: z.string().min(4, "Password must be at least 6 characters"),
});

export type ConvertType = z.infer<typeof convertSchemaInput>;
export type SignupFormData = z.infer<typeof signupSchema>;
export type LoginFormData = z.infer<typeof LoginSchema>;
