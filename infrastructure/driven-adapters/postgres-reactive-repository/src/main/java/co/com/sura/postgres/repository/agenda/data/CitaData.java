package co.com.sura.postgres.repository.agenda.data;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;
import java.time.LocalDateTime;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "cita")
public class CitaData {
    @Id
    @Column("id_cita")
    private String idCita;
    @Column("id_remision")
    private String idRemision;
    private Integer duracion;
    private Integer holgura;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm")
    @Column("fecha_inicio")
    private LocalDateTime fechaInicio;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm")
    @Column("fecha_programada")
    private LocalDateTime fechaProgramada;
    private String especialidad;
    @Column("id_regional")
    private String idRegional;
    @Column("id_estado")
    private Integer idEstado;
    @Column("id_profesional")
    private String idProfesional;
    @Column("id_conductor")
    private String idConductor;
    @Column("latitud")
    private Double latitud;
    @Column("longitud")
    private Double longitud;
}
