package co.com.sura.entity.remision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Secrecion {
    private Integer diasTratamiento;
    private boolean envioAspirador;
    private boolean visitaEnfermeria;
    private String tipoSonda;
    private boolean nasal;
    private boolean traqueostomia;
    private String tipoPrestacion;
}
