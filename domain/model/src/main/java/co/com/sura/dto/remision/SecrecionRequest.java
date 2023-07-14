package co.com.sura.dto.remision;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class SecrecionRequest {
    private String diasTratamiento;
    private boolean envioAspirador;
    private boolean visitaEnfermeria;
    private String tipoSonda;
    private boolean nasal;
    private boolean traqueostomia;
    private String tipoPrestacion;
}
