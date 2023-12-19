package co.com.sura.personal.entity;


import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;

@Getter
@Setter
@Data
@NoArgsConstructor
@ToString
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
