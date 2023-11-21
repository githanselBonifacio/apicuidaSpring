package co.com.sura.entity.personal;


import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;

@Getter
@Setter
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
public class Profesional {
    private Integer idTipoIdentificacion;
    private String numeroIdentificacion;
    private String nombres;
    private String apellidos;
    private String email;
    private String telefono;
    private String celular;
    private String direccion;
    private String genero;
    private Integer idProfesion;
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDate fechaNacimiento;
    private String idRegional;
    private boolean activo;

}
