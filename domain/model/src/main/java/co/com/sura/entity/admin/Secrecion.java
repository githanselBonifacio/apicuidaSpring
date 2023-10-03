package co.com.sura.entity.admin;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Secrecion {
    @JsonIgnore
    private Integer idSecrecion;
    private String idCita;
    private Integer diasTratamiento;
    private boolean envioAspirador;
    private boolean visitaEnfermeria;
    private String tipoSonda;
    private boolean nasal;
    private boolean traqueostomia;
    private String tipoPrestacion;
}
