package co.com.sura.remision.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class SecrecionRequest {
    private String diasTratamiento;
    private boolean envioAspirador;
    private boolean visitaEnfermeria;
    private String tipoSonda;
    private boolean nasal;
    private boolean traqueostomia;
    private String tipoPrestacion;
}
