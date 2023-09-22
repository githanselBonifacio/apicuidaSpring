package co.com.sura.postgres.repository.remision.data;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;
import java.time.LocalDate;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "remision")
public class RemisionData {
    @Id
    @Column("id_remision")
    private String idRemision;
    private String estado;
    @Column("fecha_admision")
    private LocalDate fechaAdmision;
    private String programa;
    @Column("tipo_admision")
    private String tipoAdmision;
    @Column("institucion_remite")
    private String institucionRemite;
    @Column("numero_identificacion_paciente")
    private String numeroIdentificacionPaciente;
    @Column("id_regional")
    private String idRegional;

}
